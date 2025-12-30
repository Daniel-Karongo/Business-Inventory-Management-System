import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { map, Observable } from 'rxjs';

import { environment } from '../../../../environments/environment';
import { InventoryValuationDashboard } from '../models/inventory-valuation-dashboard.model.model';
import { ApiResponse } from '../../../core/models/api-response.model';

export type CostBasis = 'WEIGHTED_AVG' | 'LAST_COST';

@Injectable({ providedIn: 'root' })
export class InventoryValuationService {

  private base = environment.apiUrl + '/inventory/valuation';

  constructor(private http: HttpClient) {}

  /** ===========================
   * DASHBOARD AGGREGATES
   * =========================== */
  getDashboard(): Observable<InventoryValuationDashboard> {
    return this.http
      .get<ApiResponse<any>>(`${this.base}/dashboard`)
      .pipe(map(res => res.data));
  }

  /** ===========================
   * TOTAL VALUATION ONLY
   * =========================== */
  getTotal(): Observable<{
    valuationMethod: string;
    totalValuation: number;
    currency: string;
  }> {
    return this.http
      .get<ApiResponse<any>>(this.base)
      .pipe(map(res => res.data));
  }

  /** ===========================
   * CATEGORY VALUATION
   * =========================== */
  getCategoryValuation(): Observable<Record<string, number>> {
    return this.http
      .get<ApiResponse<any>>(`${this.base}/categories`)
      .pipe(map(res => res.data.categories ?? {}));
  }
}