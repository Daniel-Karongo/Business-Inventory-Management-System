import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../environments/environment';
import { Observable, BehaviorSubject, tap } from 'rxjs';

@Injectable({ providedIn: 'root' })
export class AuthService {

  private http = inject(HttpClient);

  private me$ = new BehaviorSubject<MeResponse | null>(null);

  /* =========================
     LOGIN
     ========================= */
  login(payload: LoginRequest): Observable<MeResponse> {
    return this.http
      .post<MeResponse>(
        `${environment.apiUrl}/auth/login`,
        payload,
        { withCredentials: true }
      )
      .pipe(tap(me => this.me$.next(me)));
  }

  /* =========================
     CURRENT USER
     ========================= */
  loadMe(): Observable<MeResponse> {
    return this.http
      .get<MeResponse>(
        `${environment.apiUrl}/auth/me`,
        { withCredentials: true }
      )
      .pipe(tap(me => this.me$.next(me)));
  }

  getCurrentUser() {
    return this.me$.asObservable();
  }

  getSnapshot(): MeResponse | null {
    return this.me$.value;
  }

  /* =========================
     LOGOUT
     ========================= */
  logout(): Observable<void> {
    return this.http.post<void>(
      `${environment.apiUrl}/auth/logout`,
      {},
      { withCredentials: true }
    );
  }

  logoutAll(): Observable<void> {
    return this.http.post<void>(
      `${environment.apiUrl}/auth/logout-all`,
      {},
      { withCredentials: true }
    );
  }

  clearLocalState() {
    this.me$.next(null);
  }

  /* =========================
     SESSIONS
     ========================= */
  getSessions() {
    return this.http.post<any[]>(
      `${environment.apiUrl}/auth/sessions`,
      {},
      { withCredentials: true }
    );
  }
}

/* =========================
   MODELS
   ========================= */
export interface LoginRequest {
  identifier: string;
  password: string;
  branchId: string;
}

export interface MeResponse {
  userId: string;
  username: string;
  role: string;
  branchId: string;
  departmentIds: string[];
}