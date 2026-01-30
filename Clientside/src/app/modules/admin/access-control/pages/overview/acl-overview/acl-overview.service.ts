import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class AclOverviewService {

  private http = inject(HttpClient);
  private base = `${environment.apiUrl}/admin/acl`;

  getSummary() {
    return this.http.get<any>(`${this.base}/summary`);
  }

  getRecentAudits(limit = 20) {
    return this.http.get<any[]>(`${this.base}/audits`, {
      params: { limit }
    });
  }

  refreshCache() {
    return this.http.post<void>(`${this.base}/cache/refresh`, {});
  }
}