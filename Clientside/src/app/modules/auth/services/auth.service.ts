import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../environments/environment';
import { LoginRequest, LoginResponse } from '../models';

@Injectable({ providedIn: 'root' })
export class AuthService {

  private http = inject(HttpClient);

  private readonly TOKEN_KEY = 'auth_token';
  private readonly EXPIRES_KEY = 'auth_expires_at';
  private readonly USER_ROLES = 'auth_roles';

  login(payload: LoginRequest) {
    return this.http.post<LoginResponse>(`${environment.apiUrl}/auth/login`, payload);
  }

  saveSession(res: LoginResponse) {
    localStorage.setItem(this.TOKEN_KEY, res.token);
    localStorage.setItem(this.EXPIRES_KEY, String(res.expiresAt));
    localStorage.setItem(this.USER_ROLES, JSON.stringify([res.role]));

    this.setAutoLogout();
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

  getRoles(): string[] {
    if (this.isExpired()) {
      this.logout();
      return [];
    }
    return JSON.parse(localStorage.getItem(this.USER_ROLES) || '[]');
  }

  isLoggedIn(): boolean {
    const token = localStorage.getItem(this.TOKEN_KEY);
    const exp = Number(localStorage.getItem(this.EXPIRES_KEY));
    if (!token || !exp) return false;
    if (Date.now() > exp) return false;

    return true;
  }

  logout() {
    localStorage.removeItem(this.TOKEN_KEY);
    localStorage.removeItem(this.EXPIRES_KEY);
    localStorage.removeItem(this.USER_ROLES);
  }

  setAutoLogout() {
    const exp = Number(localStorage.getItem(this.EXPIRES_KEY));
    if (!exp) return;

    const timeout = exp - Date.now();
    if (timeout <= 0) {
      this.logout();
      return;
    }

    setTimeout(() => this.logout(), timeout);
  }
}

export function initAuth(auth: AuthService) {
  if (auth.isExpired()) {
    auth.logout();
  }
}