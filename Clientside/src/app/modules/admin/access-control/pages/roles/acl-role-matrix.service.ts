import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class AclRoleMatrixService {

  private http = inject(HttpClient);
  private base = `${environment.apiUrl}/admin/acl`;

  roles() {
    return this.http.get<any[]>(`${this.base}/roles`);
  }

  permissions() {
    return this.http.get<any[]>(`${this.base}/permissions`);
  }

  rolePermissions() {
    return this.http.get<any[]>(`${this.base}/role-permissions`);
  }
}