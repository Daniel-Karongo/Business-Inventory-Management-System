import { CommonModule } from '@angular/common';
import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnInit } from '@angular/core';

import { AuthService } from '../auth/services/auth.service';
import { DashboardService } from './dashboard.service';
import { DashboardSummary } from './models/dashboard-summary.model';

import { ActivityListComponent } from '../../shared/widgets/activity-list/activity-list.component';
import { RevenueLineChartComponent } from '../../shared/widgets/revenue-line-chart/revenue-line-chart.component';
import { StatCardComponent } from '../../shared/widgets/stat-card/stat-card.component';
import { FinancialDetailDialogComponent } from './dialogs/financial-detail-dialog.component';
import { MatDialog } from '@angular/material/dialog';
import { MatSelectModule } from '@angular/material/select';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatTooltipModule } from '@angular/material/tooltip';
import * as ExcelJS from 'exceljs';
import { saveAs } from 'file-saver';
import jsPDF from 'jspdf';
import { BranchService } from '../branches/services/branch.service';
import { BranchMinimalDTO } from '../branches/models/branch.model';
import { DateUtilsService } from '../../core/services/date-utils';

@Component({
  selector: 'app-dashboard',
  standalone: true,
  imports: [
    CommonModule,
    StatCardComponent,
    ActivityListComponent,
    RevenueLineChartComponent,
    MatSelectModule,
    MatFormFieldModule,
    MatButtonModule,
    MatIconModule,
    MatTooltipModule,
  ],
  templateUrl: './dashboard.component.html',
  styleUrls: ['./dashboard.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class DashboardComponent implements OnInit {

  summary?: DashboardSummary;

  financialStats: Array<{
    title: string;
    value: string;
    variance?: number;
  }> = [];
  operationalStats: Array<{ title: string; value: string }> = [];

  revenueLabels: string[] = [];
  revenueValues: number[] = [];

  profitLabels: string[] = [];
  profitValues: number[] = [];

  loading = true;
  error?: string;

  chartMode: 'REVENUE' | 'PROFIT' | 'VAT' = 'REVENUE';
  activities: any[] = [];
  selectedPeriod: string = '7D';
  selectedBranchId?: string;
  lastLoadedAt?: Date;
  branches: BranchMinimalDTO[] = [];

  constructor(
    private dashboardService: DashboardService,
    private authService: AuthService,
    private dialog: MatDialog,
    private branchService: BranchService,
    public dateUtils: DateUtilsService,
    private cdr: ChangeDetectorRef
  ) { }

  ngOnInit(): void {
    const me = this.authService.getSnapshot();

    if (!me?.branchId) {
      this.error = 'No branch selected';
      this.loading = false;
      return;
    }

    this.loadBranches();
    this.loadDashboard(me.branchId);
  }

  private loadBranches(): void {
    this.branchService.getAll(false).subscribe({
      next: branches => {
        this.branches = branches.map(b => ({
          id: b.id!,
          branchCode: b.branchCode!,
          name: b.name
        }));
        this.cdr.markForCheck();
      },
      error: () => {
        this.error = 'Failed to load branches';
        this.loading = false;

        this.cdr.markForCheck();
      }
      
    });
  }

  private getBranchName(): string {
    const branch = this.branches.find(b => b.id === this.selectedBranchId);
    return branch?.name ?? this.selectedBranchId ?? '';
  }

  get isStale(): boolean {
    if (!this.lastLoadedAt) return false;
    const diff = (Date.now() - this.lastLoadedAt.getTime()) / 1000;
    return diff > 3600; // 5 minutes
  }

  private loadDashboard(branchId: string): void {
    this.loading = true;

    this.dashboardService.getSummary(branchId).subscribe({
      next: res => {
        this.summary = res.data;
        this.buildViewModel();
        this.loading = false;
        this.lastLoadedAt = new Date();
        this.selectedBranchId = branchId;

        this.cdr.markForCheck();
      },
      error: () => {
        this.error = 'Failed to load dashboard';
        this.loading = false;

        this.cdr.markForCheck();
      }
    });
  }

  private buildViewModel(): void {
    if (!this.summary) return;

    const f = this.summary.financial;
    const o = this.summary.operational;

    /* =========================
       FINANCIAL KPIs
    ========================= */

    this.financialStats = [
      { title: 'Net Revenue Today', value: this.formatCurrency(f.netRevenueToday) },
      { title: 'Gross Profit Today', value: this.formatCurrency(f.grossProfitToday) },
      { title: 'Cash Balance', value: this.formatCurrency(f.cashBalance) },
      { title: 'VAT Payable', value: this.formatCurrency(f.vatPayable) },
      { title: 'Accounts Receivable', value: this.formatCurrency(f.accountsReceivable) },
      { title: 'Accounts Payable', value: this.formatCurrency(f.accountsPayable) },
      { title: 'Inventory Value', value: this.formatCurrency(f.inventoryValue) },
      { title: 'Corporate Tax Accrued', value: this.formatCurrency(f.corporateTaxAccrued) },
      { title: 'Gross Margin %', value: `${f.grossMarginPercent.toFixed(2)}%` },
      { title: 'Inventory Turnover', value: f.inventoryTurnover.toFixed(2) },

      // 🔥 NEW
      {
        title: 'Revenue Budget Variance',
        value: this.formatCurrency(f.revenueBudgetVariance),
        variance: f.revenueBudgetVariance
      },
      {
        title: 'Expense Budget Variance',
        value: this.formatCurrency(f.expenseBudgetVariance),
        variance: f.expenseBudgetVariance
      }
    ];

    /* =========================
       OPERATIONAL KPIs
    ========================= */

    this.operationalStats = [
      { title: 'Sales Count Today', value: o.salesCountToday.toString() },
      { title: 'Refund Count Today', value: o.refundCountToday.toString() },
      { title: 'Low Stock Items', value: o.lowStockCount.toString() },
      { title: 'Out of Stock Items', value: o.outOfStockCount.toString() },
      { title: 'Dead Stock Value', value: this.formatCurrency(o.deadStockValue) }
    ];

    this.activities = this.summary.recentActivities ?? [];
  }

  reload(): void {
    if (!this.selectedBranchId) return;
    this.loadDashboard(this.selectedBranchId);
  }

  get activeLabels(): string[] {
    if (!this.summary) return [];

    switch (this.chartMode) {
      case 'PROFIT': return this.summary.profitTrend.map(r => r.label);
      case 'VAT': return this.summary.vatTrend.map(r => r.label);
      default: return this.summary.revenueTrend.map(r => r.label);
    }
  }

  get activeValues(): number[] {
    if (!this.summary) return [];

    switch (this.chartMode) {
      case 'PROFIT': return this.summary.profitTrend.map(r => r.value);
      case 'VAT': return this.summary.vatTrend.map(r => r.value);
      default: return this.summary.revenueTrend.map(r => r.value);
    }
  }

  private formatCurrency(value: number): string {
    return `KSh ${value?.toLocaleString('en-KE') ?? '0'}`;
  }

  get arAging() {
    return this.summary?.financial.arAging;
  }

  get apAging() {
    return this.summary?.financial.apAging;
  }

  openARAging(): void {
    if (!this.arAging) return;

    this.dialog.open(FinancialDetailDialogComponent, {
      width: '420px',
      data: {
        type: 'AR_AGING',
        title: 'Accounts Receivable Aging',
        payload: this.arAging
      }
    });
  }

  openAPAging(): void {
    if (!this.apAging) return;

    this.dialog.open(FinancialDetailDialogComponent, {
      width: '420px',
      data: {
        type: 'AP_AGING',
        title: 'Accounts Payable Aging',
        payload: this.apAging
      }
    });
  }

  openBudgetVariance(): void {
    const f = this.summary?.financial;
    if (!f) return;

    this.dialog.open(FinancialDetailDialogComponent, {
      width: '420px',
      data: {
        type: 'BUDGET',
        title: 'Budget Variance Breakdown',
        payload: {
          revenue: f.revenueBudgetVariance,
          expense: f.expenseBudgetVariance
        }
      }
    });
  }

  async exportExcel(): Promise<void> {
    if (!this.summary) return;

    const workbook = new ExcelJS.Workbook();
    workbook.creator = 'Business Manager';
    workbook.created = new Date();

    const applyHeaderStyle = (row: ExcelJS.Row) => {
      row.font = { bold: true };
      row.alignment = { vertical: 'middle' };
      row.eachCell(cell => {
        cell.border = {
          top: { style: 'thin' },
          left: { style: 'thin' },
          bottom: { style: 'thin' },
          right: { style: 'thin' }
        };
      });
    };

    const applyRowBorders = (row: ExcelJS.Row) => {
      row.eachCell(cell => {
        cell.border = {
          top: { style: 'thin' },
          left: { style: 'thin' },
          bottom: { style: 'thin' },
          right: { style: 'thin' }
        };
      });
    };

    /* =========================
       FINANCIAL SHEET
    ========================= */

    const financialSheet = workbook.addWorksheet('Financial Overview');

    financialSheet.columns = [
      { header: 'Metric', key: 'metric', width: 35 },
      { header: 'Value', key: 'value', width: 22 }
    ];

    applyHeaderStyle(financialSheet.getRow(1));

    this.financialStats.forEach(s => {
      const row = financialSheet.addRow({
        metric: s.title,
        value: s.value
      });
      applyRowBorders(row);
    });

    financialSheet.getColumn('value').alignment = { horizontal: 'right' };

    /* =========================
       OPERATIONAL SHEET
    ========================= */

    const operationalSheet = workbook.addWorksheet('Operational Overview');

    operationalSheet.columns = [
      { header: 'Metric', key: 'metric', width: 35 },
      { header: 'Value', key: 'value', width: 22 }
    ];

    applyHeaderStyle(operationalSheet.getRow(1));

    this.operationalStats.forEach(s => {
      const row = operationalSheet.addRow({
        metric: s.title,
        value: s.value
      });
      applyRowBorders(row);
    });

    operationalSheet.getColumn('value').alignment = { horizontal: 'right' };

    /* =========================
       AGING SHEET
    ========================= */

    const agingSheet = workbook.addWorksheet('Aging Breakdown');

    agingSheet.columns = [
      { header: 'Type', key: 'type', width: 20 },
      { header: 'Bucket', key: 'bucket', width: 20 },
      { header: 'Value', key: 'value', width: 22 }
    ];

    applyHeaderStyle(agingSheet.getRow(1));

    const addAging = (type: string, aging: any) => {
      Object.entries(aging).forEach(([key, value]) => {
        const row = agingSheet.addRow({
          type,
          bucket: key,
          value
        });
        applyRowBorders(row);
      });
    };

    if (this.arAging) addAging('Receivables', this.arAging);
    if (this.apAging) addAging('Payables', this.apAging);

    agingSheet.getColumn('value').alignment = { horizontal: 'right' };

    /* =========================
       SAVE FILE
    ========================= */

    const buffer = await workbook.xlsx.writeBuffer();
    const blob = new Blob([buffer]);
    saveAs(blob, `dashboard-${this.summary.date}.xlsx`);
  }

  exportBoardPack(): void {
    if (!this.summary) return;

    const doc = new jsPDF();
    const pageWidth = doc.internal.pageSize.getWidth();
    const pageHeight = doc.internal.pageSize.getHeight();

    const addSectionTitle = (title: string, y: number) => {
      doc.setFont('helvetica', 'bold');
      doc.setFontSize(16);
      doc.text(title, 20, y);
      doc.setFont('helvetica', 'normal');
      doc.line(20, y + 2, pageWidth - 20, y + 2);
      return y + 10;
    };

    let y = 40;

    /* ========= COVER ========= */

    doc.setFont('helvetica', 'bold');
    doc.setFontSize(22);
    doc.text('Board Financial Pack', pageWidth / 2, 40, { align: 'center' });

    doc.setFont('helvetica', 'normal');
    doc.setFontSize(12);
    doc.text(`Branch: ${this.getBranchName()}`, pageWidth / 2, 60, { align: 'center' });
    doc.text(
      `Reporting Date: ${this.dateUtils.formatFull(this.summary.date)}`,
      pageWidth / 2,
      70,
      { align: 'center' }
    );

    doc.addPage();
    y = 30;

    /* ========= EXECUTIVE SUMMARY ========= */

    y = addSectionTitle('Executive Summary', y);

    const f = this.summary.financial;
    const o = this.summary.operational;

    const summaryLines = [
      ['Net Revenue Today', f.netRevenueToday],
      ['Gross Profit Today', f.grossProfitToday],
      ['Cash Balance', f.cashBalance],
      ['Inventory Value', f.inventoryValue],
      ['Gross Margin %', f.grossMarginPercent + '%'],
      ['Sales Today', o.salesCountToday],
      ['Low Stock Items', o.lowStockCount],
      ['Out of Stock Items', o.outOfStockCount]
    ];

    doc.setFontSize(12);

    summaryLines.forEach(line => {
      doc.text(String(line[0]), 20, y);
      doc.text(String(line[1]), pageWidth - 20, y, { align: 'right' });
      y += 8;
    });

    doc.addPage();
    y = 30;

    /* ========= FINANCIAL OVERVIEW ========= */

    y = addSectionTitle('Financial Overview', y);

    this.financialStats.forEach(stat => {
      doc.text(stat.title, 20, y);
      doc.text(stat.value, pageWidth - 20, y, { align: 'right' });
      y += 8;

      if (y > pageHeight - 20) {
        doc.addPage();
        y = 30;
      }
    });

    doc.addPage();
    y = 30;

    /* ========= OPERATIONAL OVERVIEW ========= */

    y = addSectionTitle('Operational Overview', y);

    this.operationalStats.forEach(stat => {
      doc.text(stat.title, 20, y);
      doc.text(stat.value, pageWidth - 20, y, { align: 'right' });
      y += 8;
    });

    doc.addPage();
    y = 30;

    /* ========= AGING ========= */

    y = addSectionTitle('Accounts Receivable Aging', y);

    Object.entries(this.arAging ?? {}).forEach(([k, v]) => {
      doc.text(k, 20, y);
      doc.text(`KSh ${(v as number).toLocaleString()}`, pageWidth - 20, y, { align: 'right' });
      y += 8;
    });

    y += 6;
    y = addSectionTitle('Accounts Payable Aging', y);

    Object.entries(this.apAging ?? {}).forEach(([k, v]) => {
      doc.text(k, 20, y);
      doc.text(`KSh ${(v as number).toLocaleString()}`, pageWidth - 20, y, { align: 'right' });
      y += 8;
    });

    doc.save(`board-pack-${this.summary.date}.pdf`);
  }
}