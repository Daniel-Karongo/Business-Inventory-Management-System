import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { environment } from '../../../../environments/environment';
import { BulkRequest, BulkResult } from '../../../shared/models/bulk-import.model';

@Injectable({ providedIn: 'root' })
export class SalesService {

  private base = environment.apiUrl + environment.endpoints.sales.base;

  constructor(private http: HttpClient) { }

  list(params: {
    page?: number;
    size?: number;
    status?: string;
    customer?: string;
    branchId?: string;
    from?: string;
    to?: string;
  }) {
    let httpParams = new HttpParams();

    Object.entries(params).forEach(([k, v]) => {
      if (v !== undefined && v !== null) {
        httpParams = httpParams.set(k, String(v));
      }
    });

    return this.http.get<any>(this.base, { params: httpParams });
  }

  get(id: string) {
    return this.http.get<any>(`${this.base}/${id}`);
  }

  create(payload: any) {
    return this.http.post<any>(this.base, payload);
  }

  import(
    mode: 'HISTORICAL' | 'OPERATIONAL',
    payload: BulkRequest<any>
  ) {
    const params = new HttpParams().set('mode', mode);

    return this.http.post<BulkResult<any>>(
      `${this.base}/import`,
      payload,
      { params }
    );
  }

  update(id: string, payload: any) {
    return this.http.put<any>(`${this.base}/${id}`, payload);
  }

  cancel(id: string) {
    return this.http.post<any>(
      environment.apiUrl + environment.endpoints.sales.cancel(id),
      {}
    );
  }

  refund(id: string) {
    return this.http.post<any>(
      environment.apiUrl + environment.endpoints.sales.refund(id),
      {}
    );
  }

  cancelAndRefund(id: string) {
    return this.http.post<any>(
      environment.apiUrl + environment.endpoints.sales.cancelAndRefund(id),
      {}
    );
  }

  payments(id: string) {
    return this.http.get<any[]>(
      environment.apiUrl + environment.endpoints.sales.payments(id)
    );
  }
}