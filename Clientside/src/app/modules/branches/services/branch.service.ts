import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../environments/environment';
import { Observable } from 'rxjs';
import { BranchDTO } from '../models/branch.model';
import { ApiResponse } from '../../../core/models/api-response.model';
import { BulkRequest, BulkResult } from '../../../shared/models/bulk-import.model';

@Injectable({ providedIn: 'root' })
export class BranchService {

  private http = inject(HttpClient);
  private base = `${environment.apiUrl}${environment.endpoints.branches.base}`;

  /** CREATE BRANCH */
  create(dto: BranchDTO): Observable<ApiResponse> {
    return this.http.post<ApiResponse>(this.base, dto);
  }

  // branches/services/branch.service.ts


  bulkImport(
    request: BulkRequest<any>
  ): Observable<BulkResult<BranchDTO>> {
    return this.http.post<BulkResult<BranchDTO>>(
      `${this.base}/import`,
      request
    );
  }

  /** GET ALL */
  getAll(deleted?: boolean): Observable<BranchDTO[]> {
    return this.http.get<BranchDTO[]>(this.base, {
      params: { deleted: deleted ?? false }
    });
  }

  /** GET BY ID */
  getById(id: string) {
    return this.http.get<BranchDTO>(`${this.base}/${id}`);
  }

  /** UPDATE */
  update(id: string, dto: BranchDTO): Observable<BranchDTO> {
    return this.http.patch<BranchDTO>(`${this.base}/${id}`, dto);
  }

  /** DELETE (SOFT / HARD) */
  delete(id: string, soft: boolean = true): Observable<ApiResponse> {
    return this.http.delete<ApiResponse>(`${this.base}/${id}`, {
      params: { soft }
    });
  }

  /** BULK DELETE */
  deleteBulk(ids: string[], soft = true): Observable<ApiResponse> {
    return this.http.delete<ApiResponse>(`${this.base}/bulk`, {
      body: ids,
      params: { soft }
    });
  }

  /** RESTORE */
  restore(id: string): Observable<ApiResponse> {
    return this.http.patch<ApiResponse>(`${this.base}/restore/${id}`, {});
  }

  /** RESTORE IN BULK */
  restoreBulk(ids: string[]): Observable<ApiResponse> {
    return this.http.patch<ApiResponse>(`${this.base}/restore/bulk`, ids);
  }
}