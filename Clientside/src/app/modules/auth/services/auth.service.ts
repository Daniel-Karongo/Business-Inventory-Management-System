import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../environments/environment';
import { LoginRequest, LoginResponse } from '../models';
import { IdleLogoutService } from '../../../core/services/IdleLogoutService';

@Injectable({ providedIn: 'root' })
export class AuthService {

  private http = inject(HttpClient);

  private readonly TOKEN_KEY = 'auth_token';
  private readonly EXPIRES_KEY = 'auth_expires_at';
  private readonly USER_ROLE = 'auth_role';

  login(payload: LoginRequest) {
    return this.http.post<LoginResponse>(`${environment.apiUrl}/auth/login`, payload);
  }

  saveSession(res: LoginResponse) {
    localStorage.setItem(this.TOKEN_KEY, res.token);
    localStorage.setItem(this.EXPIRES_KEY, String(res.expiresAt));
    localStorage.setItem(this.USER_ROLE, res.role);
  }

  public isExpired(): boolean {
    const expStr = localStorage.getItem(this.EXPIRES_KEY);
    if (!expStr) return true;

    const expiresAt = Number(expStr);

    // if the timestamp is invalid, expire immediately
    if (!expiresAt || Number.isNaN(expiresAt)) return true;

    return Date.now() > expiresAt;
  }

  getToken(): string | null {
    const token = localStorage.getItem(this.TOKEN_KEY);
    const exp = Number(localStorage.getItem(this.EXPIRES_KEY));
    if (!token || !exp || Date.now() > exp) return null;
    return token;
  }

  getRole(): string {
    if (this.isExpired()) {
      this.logoutFull();
      return '';
    }

    return localStorage.getItem(this.USER_ROLE) || '';
  }


  isLoggedIn(): boolean {
    const token = localStorage.getItem(this.TOKEN_KEY);
    const exp = Number(localStorage.getItem(this.EXPIRES_KEY));
    if (!token || !exp) return false;
    if (Date.now() > exp) return false;

    return true;
  }

  // auth.service.ts
  logoutRemote() {
    return this.http.post(`${environment.apiUrl}/auth/logout`, {}, {
      observe: 'response'
    });
  }

  logoutLocal() {
    localStorage.removeItem(this.TOKEN_KEY);
    localStorage.removeItem(this.EXPIRES_KEY);
    localStorage.removeItem(this.USER_ROLE);
  }

  logoutFull() {
    const token = this.getToken();

    // 🔒 No token → nothing to invalidate server-side
    if (!token) {
      this.logoutLocal();
      return;
    }

    this.logoutRemote().subscribe({
      next: () => this.logoutLocal(),
      error: () => this.logoutLocal()
    });
  }

  getSessions() {
    return this.http.post<any[]>(
      `${environment.apiUrl}/auth/sessions`,
      {}
    );
  }

  logoutAllRemote() {
    return this.http.post(`${environment.apiUrl}/auth/logout-all`, {}, {
      observe: 'response'
    });
  }
}

export function initAuth(auth: AuthService) {
  if (auth.isExpired()) {
    auth.logoutLocal(); // ✅ local cleanup only
  }
}