import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../environments/environment';

export type ResetChannel = 'EMAIL' | 'SMS' | 'IDENTITY';

@Injectable({ providedIn: 'root' })
export class PasswordResetService {

  private http = inject(HttpClient);
  private base = environment.apiUrl;

  /* =====================================
     OPTIONS
     ===================================== */

  getOptions() {
    return this.http.get<{
      channels: ResetChannel[];
      identityFields: string[];
    }>(`${this.base}${environment.endpoints.auth.passwordReset.options}`);
  }

  /* =====================================
     INITIATE RESET
     ===================================== */

  initiate(payload: {
    identifier: string;
    channel: ResetChannel;
  }) {
    return this.http.post(
      `${this.base}${environment.endpoints.auth.passwordReset.initiate}`,
      payload
    );
  }

  /* =====================================
     COMPLETE RESET
     ===================================== */

  completeWithToken(payload: {
    token: string;
    newPassword: string;
  }) {
    return this.http.post(
      `${this.base}${environment.endpoints.auth.passwordReset.complete}`,
      payload
    );
  }

  completeWithIdentity(payload: {
    identifier: string;
    idNumber?: string;
    email?: string;
    phone?: string;
    newPassword: string;
  }) {
    return this.http.post(
      `${this.base}${environment.endpoints.auth.passwordReset.complete}`,
      payload
    );
  }
}