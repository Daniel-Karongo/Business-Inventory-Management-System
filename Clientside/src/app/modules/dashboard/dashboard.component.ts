import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';

import { DashboardService } from './dashboard.service';
import { AuthService } from '../auth/services/auth.service';

import { StatCardComponent } from '../../shared/widgets/stat-card/stat-card.component';
import { ActivityListComponent } from '../../shared/widgets/activity-list/activity-list.component';
import { RevenueLineChartComponent } from '../../shared/widgets/revenue-line-chart/revenue-line-chart.component';
import { SalesCategoryChartComponent } from '../../shared/widgets/sales-category-chart/sales-category-chart.component';

@Component({
  selector: 'app-dashboard',
  standalone: true,
  imports: [
    CommonModule,
    StatCardComponent,
    ActivityListComponent,
    RevenueLineChartComponent,
    SalesCategoryChartComponent
  ],
  templateUrl: './dashboard.component.html',
  styleUrls: ['./dashboard.component.scss']
})
export class DashboardComponent implements OnInit {

  /* =========================
     VIEW STATE
  ========================= */

  stats: Array<{ title: string; value: string; change: string }> = [];

  salesByCategory: Array<{ label: string; value: number }> = [];
  revenueTrend: Array<{ label: string; value: number }> = [];

  activities: Array<{
    type: string;
    description: string;
    actor?: string;
    time: string;
  }> = [];

  revenueLabels: string[] = [];
  revenueValues: number[] = [];

  categoryLabels: string[] = [];
  categoryValues: number[] = [];

  loading = true;
  error?: string;

  constructor(
    private dashboardService: DashboardService,
    private authService: AuthService
  ) {}

  ngOnInit(): void {
    const me = this.authService.getSnapshot();

    if (!me?.branchId) {
      this.error = 'No branch selected';
      this.loading = false;
      return;
    }

    this.loadDashboard(me.branchId);
  }

  /* =========================
     DATA LOAD
  ========================= */

  private loadDashboard(branchId: string): void {
    this.loading = true;

    this.dashboardService.getSummary(branchId).subscribe({
      next: res => {
        const data = res?.data;
        if (!data) {
          this.error = 'Invalid dashboard response';
          this.loading = false;
          return;
        }

        /* ---------- KPIs ---------- */
        this.stats = [
          {
            title: 'Sales Today',
            value: this.formatCurrency(data.kpis.salesToday),
            change: ''
          },
          {
            title: 'Inventory Value',
            value: this.formatCurrency(data.kpis.inventoryValue),
            change: ''
          },
          {
            title: 'Customers',
            value: String(data.kpis.customers),
            change: ''
          },
          {
            title: 'Sales Count',
            value: String(data.kpis.salesCount),
            change: ''
          }
        ];

        /* ---------- CHART DATA ---------- */
        this.salesByCategory = data.salesByCategory ?? [];
        this.revenueTrend = this.aggregateByDate(data.revenueTrend ?? []);

        this.revenueLabels = this.revenueTrend.map(r => r.label);
        this.revenueValues = this.revenueTrend.map(r => r.value);

        this.categoryLabels = this.salesByCategory.map(c => c.label);
        this.categoryValues = this.salesByCategory.map(c => c.value);

        /* ---------- ACTIVITY ---------- */
        this.activities = (data.recentActivities ?? []).map((a: any) => ({
          type: a.type,
          description: a.description,
          actor: a.actor,
          time: this.relativeTime(a.time)
        }));

        this.loading = false;
      },
      error: err => {
        console.error('Dashboard load failed', err);
        this.error = 'Failed to load dashboard';
        this.loading = false;
      }
    });
  }

  /* =========================
     HELPERS
  ========================= */

  private aggregateByDate(
    points: { label: string; value: number }[]
  ): { label: string; value: number }[] {

    const map = new Map<string, number>();

    for (const p of points) {
      map.set(p.label, (map.get(p.label) ?? 0) + p.value);
    }

    return Array.from(map.entries())
      .map(([label, value]) => ({ label, value }))
      .sort((a, b) => a.label.localeCompare(b.label));
  }

  private formatCurrency(value: number): string {
    return `KSh ${value?.toLocaleString('en-KE') ?? '0'}`;
  }

  private relativeTime(iso: string): string {
    const then = new Date(iso).getTime();
    const diff = Math.floor((Date.now() - then) / 1000);

    if (diff < 60) return `${diff}s ago`;
    if (diff < 3600) return `${Math.floor(diff / 60)}m ago`;
    if (diff < 86400) return `${Math.floor(diff / 3600)}h ago`;
    return `${Math.floor(diff / 86400)}d ago`;
  }
}