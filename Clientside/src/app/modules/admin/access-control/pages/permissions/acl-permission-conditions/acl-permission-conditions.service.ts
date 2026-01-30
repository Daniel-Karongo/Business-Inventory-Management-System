import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class AclPermissionConditionsService {

  private http = inject(HttpClient);
  private base = `${environment.apiUrl}/admin/acl`;

  listPermissions() {
    return this.http.get<any[]>(`${this.base}/permissions`);
  }

  listRoles() {
    return this.http.get<any[]>(`${this.base}/roles`);
  }

  listConditions(permissionId: string, roleName: string) {
    return this.http.get<any[]>(
      `${this.base}/permissions/${permissionId}/conditions`,
      { params: { role: roleName } }
    );
  }

  addCondition(permissionId: string, payload: {
    role: string;
    param: string;
    operator: 'EQ';
    value: string;
  }) {
    return this.http.post(
      `${this.base}/permissions/${permissionId}/conditions`,
      payload
    );
  }

  softDeleteCondition(conditionId: string, reason: string) {
    return this.http.delete(
      `${this.base}/conditions/${conditionId}`,
      { body: { reason } }
    );
  }
}