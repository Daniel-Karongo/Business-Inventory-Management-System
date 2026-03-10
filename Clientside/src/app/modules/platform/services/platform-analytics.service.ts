import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../environments/environment';
import { Observable } from 'rxjs';

export interface TenantUsageMetric {

  id: string;

  tenantId: string;

  requests: number;

  errors: number;

  snapshotTime: string;

}

export interface MetricsPage {

  content: TenantUsageMetric[];

  totalElements: number;

}

@Injectable({ providedIn: 'root' })
export class PlatformAnalyticsService {

  private http = inject(HttpClient);

  private base = `${environment.apiUrl}/platform/analytics`;

  getMetrics(
    page = 0,
    size = 50
  ): Observable<MetricsPage> {

    return this.http.get<MetricsPage>(
      `${this.base}?page=${page}&size=${size}`
    );

  }

}