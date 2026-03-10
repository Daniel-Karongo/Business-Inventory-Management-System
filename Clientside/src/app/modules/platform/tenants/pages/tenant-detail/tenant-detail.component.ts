import { Component, inject, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ActivatedRoute, RouterModule } from '@angular/router';

import { TenantService } from '../../services/tenant.service';
import { TenantResponse } from '../../models/tenant.model';

import {
  PlatformAnalyticsService,
  TenantUsageMetric
} from '../../../services/platform-analytics.service';

import { MatCardModule } from '@angular/material/card';
import { MatButtonModule } from '@angular/material/button';
import { MatTableModule } from '@angular/material/table';

@Component({
  standalone: true,
  selector: 'app-tenant-detail',
  templateUrl: './tenant-detail.component.html',
  styleUrls: ['./tenant-detail.component.scss'],
  imports: [
    CommonModule,
    RouterModule,
    MatCardModule,
    MatButtonModule,
    MatTableModule
  ]
})
export class TenantDetailComponent implements OnInit {

  private route = inject(ActivatedRoute);

  private tenantService = inject(TenantService);

  private analytics = inject(PlatformAnalyticsService);

  tenant?: TenantResponse;

  metrics: TenantUsageMetric[] = [];

  tenantId!: string;

  displayedColumns = [
    'requests',
    'errors',
    'snapshotTime'
  ];

  ngOnInit() {

    this.tenantId = this.route.snapshot.paramMap.get('id')!;

    this.loadTenant();

    this.loadMetrics();

  }

  loadTenant() {

    this.tenantService
      .getTenant(this.tenantId)
      .subscribe(t => this.tenant = t);

  }

  loadMetrics() {

    this.analytics
      .getMetrics(0, 50)
      .subscribe(page => {

        this.metrics = page.content
          .filter(m => m.tenantId === this.tenantId)
          .slice(0, 10);

      });

  }

}