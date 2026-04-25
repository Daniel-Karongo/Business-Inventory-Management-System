import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { TenantResponse, TenantCreateRequest, TenantPage } from '../models/tenant.model';
import { environment } from '../../../../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class TenantService {

  private http = inject(HttpClient);
  private base = `${environment.apiUrl}/platform/tenants`;

  getTenants(
    page = 0,
    size = 20,
    search = ''
  ): Observable<TenantPage> {

    let url =
      `${this.base}?page=${page}&size=${size}`;

    if (search?.trim()) {
      url += `&search=${encodeURIComponent(
        search.trim()
      )}`;
    }

    return this.http.get<TenantPage>(url);
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
