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

  constructor(private http: HttpClient) { }

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

  // ===========================
  // BATCH ENDPOINTS
  // ===========================

  getBatches(variantId: string, branchId: string) {
    return this.http.get<any>(
      `${environment.apiUrl}/inventory/variant/${variantId}/branch/${branchId}/batches`
    );
  }

  suggestBatches(variantId: string, branchId: string, quantity: number) {
    return this.http.get<any>(
      `${environment.apiUrl}/inventory/variant/${variantId}/branch/${branchId}/suggest-batches?quantity=${quantity}`
    );
  }

  getBatchConsumptions(batchId: string) {
    return this.http
      .get<ApiResponse<any[]>>(
        `${this.base}/batch/${batchId}/consumptions`
      )
      .pipe(map(res => res.data ?? []));
  }

  getFifoPrice(variantId: string, branchId: string) {
    return this.http
      .get<ApiResponse<number>>(
        `${this.base}/variant/${variantId}/branch/${branchId}/fifo-price`
      )
      .pipe(map(res => res.data ?? 0));
  }

  getAllBatches(variantId: string, branchId: string) {
    return this.http
      .get<ApiResponse<any[]>>(
        `${this.base}/variant/${variantId}/branch/${branchId}/batches`
      )
      .pipe(map(res => res.data ?? []));
  }
  
  // ===========================
  // PREVIEW ALLOCATION
  // ===========================

  previewAllocation(payload: {
    variantId: string;
    branchId: string;
    quantity: number;
    selectedBatchIds?: string[] | null;
  }) {
    return this.http.post<ApiResponse<any>>(
      `${this.base}/preview-allocation`,
      payload
    ).pipe(map(res => res.data));
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

  bulkImport(request: any) {
    return this.http.post<any>(
      `${this.base}/import`,
      request
    );
  }
}