import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../environments/environment';
import { map, Observable } from 'rxjs';
import { ApiResponse } from '../../../core/models/api-response.model';
import { InventoryResponse } from '../models/inventory-response.model';
import { StockTransactionDTO } from '../models/stock-transaction.model';

@Injectable({ providedIn: 'root' })
export class InventoryService {

  private base = environment.apiUrl + environment.endpoints.inventory.base;

  constructor(private http: HttpClient) {}

  /** ===========================
   * READ — NORMALIZED
   * =========================== */

  getAll(): Observable<InventoryResponse[]> {
    return this.http
      .get<ApiResponse<InventoryResponse[]>>(this.base)
      .pipe(map(res => res.data ?? []));
  }

  getByBranch(branchId: string): Observable<InventoryResponse[]> {
    return this.http
      .get<ApiResponse<InventoryResponse[]>>(
        `${this.base}/branch/${branchId}`
      )
      .pipe(map(res => res.data ?? []));
  }

  /** Used by Products → Variant List */
  getVariantAcrossBranches(
    variantId: string
  ): Observable<InventoryResponse[]> {
    return this.http
      .get<ApiResponse<InventoryResponse[]>>(
        `${this.base}/variant/${variantId}`
      )
      .pipe(map(res => res.data ?? []));
  }

  /** Used by Sales → Sale Create */
  getVariantStock(
    variantId: string,
    branchId: string
  ): Observable<InventoryResponse | null> {
    return this.http
      .get<ApiResponse<InventoryResponse>>(
        `${this.base}/variant/${variantId}/branch/${branchId}`
      )
      .pipe(map(res => res.data ?? null));
  }

  /** ===========================
   * TRANSACTIONS (already raw)
   * =========================== */

  getTransactionsByVariant(
    variantId: string
  ): Observable<StockTransactionDTO[]> {
    return this.http.get<StockTransactionDTO[]>(
      `${environment.apiUrl}/stock/transactions/variant/${variantId}`
    );
  }

  /** ===========================
   * MUTATIONS (unchanged)
   * =========================== */

  receiveStock(payload: any): Observable<ApiResponse> {
    return this.http.post<ApiResponse>(`${this.base}/receive`, payload);
  }

  adjustVariantStock(payload: any): Observable<ApiResponse> {
    return this.http.post<ApiResponse>(`${this.base}/adjust/variant`, payload);
  }

  transferStock(payload: any): Observable<ApiResponse> {
    return this.http.post<ApiResponse>(`${this.base}/transfer`, payload);
  }

  bulkReceiveStock(payload: {
    items: any[];
  }): Observable<ApiResponse> {
    return this.http.post<ApiResponse>(
      `${this.base}/receive/bulk`,
      payload
    );
  }
}