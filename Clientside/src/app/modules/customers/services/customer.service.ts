import { Injectable, inject } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { environment } from '../../../../environments/environment';
import { CustomerRequest, CustomerResponse } from '../models/customer.model';
import { forkJoin } from 'rxjs';
import { BulkRequest, BulkResult } from '../../../shared/models/bulk-import.model';

@Injectable({ providedIn: 'root' })
export class CustomerService {

  private http = inject(HttpClient);
  private base = `${environment.apiUrl}${environment.endpoints.customers.base}`;

  list(
    page = 0,
    size = 20,
    deleted?: any,
    type?: string,
    gender?: string
  ) {
    let params = new HttpParams()
      .set('page', page)
      .set('size', size)

    if (deleted != undefined) params.set('deleted', deleted)
    if (type) params = params.set('type', type);
    if (gender) params = params.set('gender', gender);

    return this.http.get<any>(this.base, { params });
  }

  exportCsv(
    q?: string,
    type?: string,
    gender?: string,
    deleted?: boolean
  ) {
    let params = new HttpParams();

    if (q) params = params.set('q', q);
    if (type) params = params.set('type', type);
    if (gender) params = params.set('gender', gender);
    if (deleted !== undefined) params = params.set('deleted', String(deleted));

    return this.http.get(`${this.base}/export`, {
      params,
      responseType: 'blob'
    });
  }

  get(id: string) {
    return this.http.get<CustomerResponse>(`${this.base}/${id}`);
  }

  lookupByPhone(phone: string) {
    return this.http.get<any>(
      `${this.base}/lookup`,
      { params: { phone } }
    );
  }

  create(req: CustomerRequest) {
    return this.http.post<CustomerResponse>(this.base, req);
  }

  bulkImport(
    request: BulkRequest<any>
  ) {
    return this.http.post<BulkResult<CustomerResponse>>(
      `${this.base}/import`,
      request
    );
  }

  update(id: string, req: CustomerRequest) {
    return this.http.put<CustomerResponse>(`${this.base}/${id}`, req);
  }

  payments(id: string) {
    return this.http.get<any[]>(`${this.base}/${id}/payments`);
  }

  sales(id: string) {
    return this.http.get<any[]>(`${this.base}/${id}/sales`);
  }

  softDelete(id: string, reason: string | null) {
    return this.http.delete(`${this.base}/${id}`, {
      params: { soft: true },
      body: reason ?? ''
    });
  }

  restore(id: string, reason: string | null) {
    return this.http.patch(`${this.base}/${id}/restore`, reason ?? '');
  }

  hardDelete(id: string, reason: string | null) {
    return this.http.delete<void>(
      `${this.base}/${id}`,
      {
        params: { soft: 'false' },
        body: reason ?? ''
      }
    );
  }

  softDeleteBulk(ids: string[], reason: string | null) {
    return this.http.post(`${this.base}/bulk/soft-delete`, {
      ids,
      reason: reason ?? ''
    });
  }

  restoreBulk(ids: string[], reason: string | null) {
    return this.http.post(`${this.base}/bulk/restore`, {
      ids,
      reason: reason ?? ''
    });
  }

  hardDeleteBulk(ids: string[], reason: string | null) {
    return this.http.post(`${this.base}/bulk/hard-delete`, {
      ids,
      reason: reason ?? ''
    });
  }
}