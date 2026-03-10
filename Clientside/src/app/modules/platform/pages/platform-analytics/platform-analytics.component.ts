import { Component, inject, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';

import { PlatformAnalyticsService, TenantUsageMetric } from '../../services/platform-analytics.service';

import { MatTableModule } from '@angular/material/table';
import { MatCardModule } from '@angular/material/card';

@Component({
  standalone: true,
  selector: 'app-platform-analytics',
  templateUrl: './platform-analytics.component.html',
  styleUrls: ['./platform-analytics.component.scss'],
  imports: [
    CommonModule,
    MatTableModule,
    MatCardModule
  ]
})
export class PlatformAnalyticsComponent implements OnInit {

  private analytics = inject(PlatformAnalyticsService);

  metrics: TenantUsageMetric[] = [];

  displayedColumns = [
    'tenantId',
    'requests',
    'errors',
    'snapshotTime'
  ];

  ngOnInit() {

    this.load();

  }

  load() {

    this.analytics.getMetrics().subscribe(page => {

      this.metrics = page.content ?? [];

    });

  }

}