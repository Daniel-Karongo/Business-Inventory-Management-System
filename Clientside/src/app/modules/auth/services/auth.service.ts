import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../environments/environment';
import { Observable, BehaviorSubject, tap, catchError, shareReplay, of, map, filter, take } from 'rxjs';
import { DeviceService } from '../../../core/services/device.service';

@Injectable({ providedIn: 'root' })
export class AuthService {

  private http = inject(HttpClient);
  private deviceService = inject(DeviceService);

  private me$ = new BehaviorSubject<MeResponse | null>(null);
  private RETRY_KEY = 'auth_retry_payload';
  private RETRY_TTL_MS = 10 * 60 * 1000; // 10 minutes
  private lastLoginRequestMemory: LoginRequest | null = null;
  private initialized = false;
  private init$?: Observable<MeResponse | null>;
  private loaded = false;


  init(): Observable<MeResponse | null> {
    if (this.initialized && this.init$) {
      return this.init$;
    }

    this.init$ = this.loadMe().pipe(
      tap(() => {
        this.initialized = true;
        this.loaded = true;
      }),
      shareReplay(1)
    );

    return this.init$;
  }

  /* =========================
     LOGIN
     ========================= */
  login(payload: LoginRequest): Observable<MeResponse> {
    return this.http
      .post<MeResponse>(
        `${environment.apiUrl}/auth/login`,
        payload,
        {
          withCredentials: true,
          observe: 'response'
        }
      )
      .pipe(
        tap(res => {
          this.me$.next(res.body!);
        }),
        map(res => res.body!)
      );
  }

  /* =========================
     CURRENT USER
     ========================= */
  loadMe(): Observable<MeResponse | null> {
    return this.http.get<MeResponse>(`${environment.apiUrl}/auth/me`, {
      withCredentials: true
    }).pipe(
      tap(me => this.me$.next(me)),
      catchError(() => {
        this.me$.next(null);
        return of(null);
      })
    );
  }

  setUser(me: MeResponse) {
    this.me$.next(me);
  }

  getCurrentUser() {
    return this.me$.pipe(
      take(1)
    );
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
    ).pipe(tap(() => this.clearLocalState()));
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

  /* =========================
   BIOMETRICS
   ========================= */

  biometricChallenge(deviceId: string) {
    return this.http.post<any>(
      `${environment.apiUrl}/auth/biometric/challenge?deviceId=${deviceId}`,
      {},
      { withCredentials: true }
    );
  }

  biometricVerify(payload: any) {
    return this.http.post<MeResponse>(
      `${environment.apiUrl}/auth/biometric/verify`,
      payload,
      { withCredentials: true }
    ).pipe(tap(me => this.me$.next(me)));
  }

  /* =========================
   BIOMETRIC REGISTRATION
   ========================= */

  biometricRegisterStart(deviceId: string) {
    return this.http.post<any>(
      `${environment.apiUrl}/auth/biometric/register/start`,
      { deviceId },
      { withCredentials: true }
    );
  }

  biometricRegisterFinish(credential: any, deviceId: string) {
    return this.http.post<void>(
      `${environment.apiUrl}/auth/biometric/register/finish?deviceId=${deviceId}`,
      credential,
      {
        withCredentials: true,
        headers: {
          'Content-Type': 'application/json' // ✅ FORCE JSON
        }
      }
    );
  }

  isAuthenticated(): boolean {
    return this.me$.value !== null;
  }
}

/* =========================
   MODELS
   ========================= */
export interface LoginRequest {
  identifier: string;
  password: string;
  branchId: string;
  deviceId: string;
  latitude: number;
  longitude: number;
  accuracy: number;
}

export interface MeResponse {
  userId: string;
  username: string;
  role: string;
  branchId: string;
  departmentIds: string[];
  userType: 'PLATFORM' | 'TENANT';
}