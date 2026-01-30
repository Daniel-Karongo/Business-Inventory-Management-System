import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class AclEndpointPermissionsService {

  private http = inject(HttpClient);
  private base = `${environment.apiUrl}/admin/acl`;

  endpoints() {
    return this.http.get<any[]>(`${this.base}/endpoints`);
  }

  permissions() {
    return this.http.get<any[]>(`${this.base}/permissions`);
  }

  reassign(payload: {
    endpointId: number;
    permissionId: string;
  }) {
    return this.http.post(`${this.base}/endpoints`, payload);
  }

  remove(endpointId: number) {
    return this.http.delete(`${this.base}/endpoints/${endpointId}`);
  }
}