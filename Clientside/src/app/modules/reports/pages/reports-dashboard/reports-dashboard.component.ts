import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';
import { MatIconModule } from '@angular/material/icon';

import { ReportDialogComponent } from '../../components/report-dialog/report-dialog.component';
import { ReportDefinition } from '../../models/report.model';

@Component({
  selector: 'app-reports-dashboard',
  standalone: true, // ✅ REQUIRED
  templateUrl: './reports-dashboard.component.html',
  styleUrls: ['./reports-dashboard.component.scss'], // ✅ REQUIRED
  imports: [
    CommonModule,
    MatDialogModule,
    MatIconModule
  ]
})
export class ReportsDashboardComponent {

  reports: ReportDefinition[] = [
    { key: 'trial_balance', title: 'Trial Balance', description: 'Balances by account' },
    { key: 'profit_and_loss', title: 'Profit & Loss', description: 'Income vs expenses' },
    { key: 'general_ledger', title: 'General Ledger', description: 'Account ledger', requiresAccount: true },
    { key: 'sales_summary', title: 'Sales Summary', description: 'Sales overview' }
  ];

  constructor(private dialog: MatDialog) { }

  open(report: ReportDefinition) {
    this.dialog.open(ReportDialogComponent, {
      width: '420px',
      maxWidth: '95vw',
      autoFocus: false,
      data: report
    });
  }
}