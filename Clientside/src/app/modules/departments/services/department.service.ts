import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../environments/environment';
import { Observable } from 'rxjs';
import { DepartmentDTO, DepartmentMinimalDTO } from '../models/department.model';
import { ApiResponse } from '../../../core/models/api-response.model';
import { BulkRequest, BulkResult } from '../../../shared/models/bulk-import.model';

@Injectable({ providedIn: 'root' })
export class DepartmentService {

  private http = inject(HttpClient);
  private base = `${environment.apiUrl}${environment.endpoints.departments.base}`;

  /** CREATE */
  create(dto: DepartmentDTO): Observable<DepartmentDTO> {
    return this.http.post<DepartmentDTO>(this.base, dto);
  }

  bulkImport(
    request: BulkRequest<any>
  ): Observable<BulkResult<DepartmentDTO>> {
    return this.http.post<BulkResult<DepartmentDTO>>(
      `${this.base}/import`,
      request
    );
  }

  /** UPDATE */
  update(id: string, dto: DepartmentDTO): Observable<DepartmentDTO> {
    return this.http.put<DepartmentDTO>(`${this.base}/${id}`, dto);
  }

  /** GET */
  get(id: string): Observable<DepartmentDTO> {
    return this.http.get<DepartmentDTO>(`${this.base}/${id}`);
  }

  /** GET ALL */
  getAll(deleted?: boolean): Observable<DepartmentDTO[]> {
    return this.http.get<DepartmentDTO[]>(this.base, {
      params: { deleted: deleted ?? false }
    });
  }

  /** GET DEPARTMENTS FOR USER */
  getUserDepartments(userId: string): Observable<DepartmentMinimalDTO[]> {
    return this.http.get<DepartmentMinimalDTO[]>(`${this.base}/user/${userId}`);
  }

  /** DELETE (SOFT / HARD) */
  delete(id: string, soft = true): Observable<ApiResponse> {
    return this.http.delete<ApiResponse>(`${this.base}/delete/${id}`, {
      params: { soft }
    });
  }

  /** DELETE BULK */
  deleteBulk(ids: string[], soft = true): Observable<ApiResponse> {
    return this.http.delete<ApiResponse>(`${this.base}/delete/bulk`, {
      body: ids,
      params: { soft }
    });
  }

  /** RESTORE */
  restore(id: string): Observable<ApiResponse> {
    return this.http.patch<ApiResponse>(`${this.base}/restore/${id}`, {});
  }

  /** RESTORE BULK */
  restoreBulk(ids: string[]): Observable<ApiResponse> {
    return this.http.patch<ApiResponse>(`${this.base}/restore/bulk`, ids);
  }

  /** AUDITS */
  getAudits(id: string) {
    return this.http.get(`${this.base}/${id}/audits`);
  }

  getAllAudits() {
    return this.http.get(`${this.base}/all/audits`);
  }

  getAuditsByPerformer(id: string) {
    return this.http.get(`${this.base}/audits/performer/${id}`);
  }
}