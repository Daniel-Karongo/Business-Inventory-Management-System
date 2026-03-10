import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../../environments/environment';
import { Observable } from 'rxjs';
import { TenantResponse, TenantCreateRequest } from '../models/tenant.model';

@Injectable({ providedIn: 'root' })
export class TenantService {

  private http = inject(HttpClient);
  private base = `${environment.apiUrl}/platform/tenants`;

  getTenants(
    page = 0,
    size = 20,
    search = ''
  ): Observable<any> {

    let url = `${this.base}?page=${page}&size=${size}`;

    if (search) {
      url += `&search=${search}`;
    }

    return this.http.get(url);

  }

  getTenant(id: string): Observable<TenantResponse> {
    return this.http.get<TenantResponse>(`${this.base}/${id}`);
  }

  createTenant(payload: TenantCreateRequest): Observable<TenantResponse> {
    return this.http.post<TenantResponse>(this.base, payload);
  }

  suspendTenant(id: string) {
    return this.http.post(`${environment.apiUrl}/platform/admin/tenants/${id}/suspend`, {});
  }

  activateTenant(id: string) {
    return this.http.post(`${environment.apiUrl}/platform/admin/tenants/${id}/activate`, {});
  }

  expireTenant(id: string) {
    return this.http.post(`${environment.apiUrl}/platform/admin/tenants/${id}/expire`, {});
  }

}