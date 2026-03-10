import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../environments/environment';
import { Observable } from 'rxjs';

export interface AuditLog {

  id: string;

  tenantId: string;

  userId: string;

  entityType: string;

  entityId: string;

  action: string;

  timestamp: string;

}

@Injectable({ providedIn: 'root' })
export class PlatformAuditService {

  private http = inject(HttpClient);

  private base = `${environment.apiUrl}/platform/audit`;

  getAudit(page = 0, size = 10): Observable<any> {

    return this.http.get(
      `${this.base}?page=${page}&size=${size}`
    );

  }

}