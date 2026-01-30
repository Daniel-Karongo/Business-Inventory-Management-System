import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';

@Injectable({ providedIn: 'root' })
export class AclAdminService {

  constructor(private http: HttpClient) {}

  apply(changes: any[], reason: string) {
    return this.http.post(
      '/api/admin/acl/role-permissions/bulk',
      { changes, reason }
    );
  }
}