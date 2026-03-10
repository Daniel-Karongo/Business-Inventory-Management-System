import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../../environments/environment';
import { Observable } from 'rxjs';

@Injectable({ providedIn: 'root' })
export class TenantSubscriptionService {

  private http = inject(HttpClient);

  private base = `${environment.apiUrl}/platform/admin/tenants`;

  assignPlan(tenantId: string, planCode: string): Observable<void> {

    return this.http.post<void>(
      `${this.base}/${tenantId}/plan/${planCode}`,
      {}
    );

  }

}