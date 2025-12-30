import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../environments/environment';
import { Observable } from 'rxjs';
import { ApiResponse } from '../../../core/models/api-response.model';
import { InventoryResponse } from '../models/inventory-response.model';
import { StockTransactionDTO } from '../models/stock-transaction.model';

@Injectable({ providedIn: 'root' })
export class InventoryService {

  private base = environment.apiUrl + environment.endpoints.inventory.base;

  constructor(private http: HttpClient) { }

  /** ===========================
   * READ (GLOBAL / FILTERED)
   * =========================== */

  getAll(): Observable<ApiResponse<InventoryResponse[]>> {
    return this.http.get<ApiResponse<InventoryResponse[]>>(this.base);
  }

  getByBranch(branchId: string): Observable<ApiResponse<InventoryResponse[]>> {
    return this.http.get<ApiResponse<InventoryResponse[]>>(
      `${this.base}/branch/${branchId}`
    );
  }

  getLowStock(threshold: number = 10): Observable<ApiResponse<InventoryResponse[]>> {
    return this.http.get<ApiResponse<InventoryResponse[]>>(
      `${this.base}/low-stock`,
      { params: { threshold } }
    );
  }

  getOutOfStock(): Observable<ApiResponse<InventoryResponse[]>> {
    return this.http.get<ApiResponse<InventoryResponse[]>>(
      `${this.base}/out-of-stock`
    );
  }

  getVariantAcrossBranches(variantId: string) {
    return this.http.get<ApiResponse<InventoryResponse[]>>(
      `${this.base}/variant/${variantId}`
    );
  }

  getTransactionsByVariant(variantId: string) {
    return this.http.get<StockTransactionDTO[]>(
      `${environment.apiUrl}/stock/transactions/variant/${variantId}`
    );
  }

  /** ===========================
   * MUTATIONS (ROLE-GATED)
   * =========================== */

  receiveStock(payload: any) {
    return this.http.post<ApiResponse>(
      `${this.base}/receive`,
      payload
    );
  }

  adjustVariantStock(payload: any) {
    return this.http.post<ApiResponse>(
      `${this.base}/adjust/variant`,
      payload
    );
  }
}