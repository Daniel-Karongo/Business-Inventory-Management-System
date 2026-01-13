import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { environment } from '../../../../environments/environment';
import { Observable } from 'rxjs';

@Injectable({ providedIn: 'root' })
export class AccountsService {

  private base = `${environment.apiUrl}/accounts`;

  constructor(private http: HttpClient) {}

  /* ============================================================
     READ
  ============================================================ */

  list(): Observable<any[]> {
    return this.http.get<any[]>(this.base);
  }

  get(id: string): Observable<any> {
    return this.http.get<any>(`${this.base}/${id}`);
  }

  /* ============================================================
     WRITE (ADMIN+)
  ============================================================ */

  create(payload: {
    code: string;
    name: string;
    type: 'ASSET' | 'LIABILITY' | 'EQUITY' | 'INCOME' | 'EXPENSE';
  }): Observable<any> {
    return this.http.post(`${this.base}/admin`, payload);
  }

  rename(
    id: string,
    payload: { name: string }
  ): Observable<void> {
    return this.http.put<void>(`${this.base}/admin/${id}`, payload);
  }

  /* ============================================================
     STATUS (ENTERPRISE-SAFE)
  ============================================================ */

  deactivate(id: string): Observable<void> {
    return this.http.post<void>(`${this.base}/admin/${id}/deactivate`, {});
  }

  activate(id: string): Observable<void> {
    return this.http.post<void>(`${this.base}/admin/${id}/activate`, {});
  }
}