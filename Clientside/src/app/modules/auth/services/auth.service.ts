import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../environments/environment';
import { Observable, BehaviorSubject, tap, catchError, shareReplay, of, map } from 'rxjs';
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


  init(): Observable<MeResponse | null> {
    if (this.initialized && this.init$) {
      return this.init$;
    }

    this.init$ = this.loadMe().pipe(
      catchError(() => {
        this.clearLocalState();
        return of(null);
      }),
      tap(() => this.initialized = true),
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
  loadMe(): Observable<MeResponse> {
    return this.http
      .get<MeResponse>(
        `${environment.apiUrl}/auth/me`,
        { withCredentials: true }
      )
      .pipe(tap(me => this.me$.next(me)));
  }

  setUser(me: MeResponse) {
    this.me$.next(me);
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

  biometricRegisterStart(payload: LoginRequest) {
    return this.http.post<any>(
      `${environment.apiUrl}/auth/biometric/register/start`,
      payload,
      { withCredentials: true }
    );
  }

  biometricRegisterFinish(rawJson: string, deviceId: string) {
    return this.http.post<void>(
      `${environment.apiUrl}/auth/biometric/register/finish?deviceId=${deviceId}`,
      rawJson,
      {
        withCredentials: true,
        headers: { 'Content-Type': 'application/json' }
      }
    );
  }

  /* =========================
   POLLING
   ========================= */

  setLastLoginRequest(req: LoginRequest) {

    this.lastLoginRequestMemory = req;

    const data = {
      payload: req,
      createdAt: Date.now()
    };

    sessionStorage.setItem(this.RETRY_KEY, JSON.stringify(data));
  }

  getLastLoginRequest(): LoginRequest | null {

    // 1. Memory (fast path)
    if (this.lastLoginRequestMemory) {
      return this.lastLoginRequestMemory;
    }

    // 2. SessionStorage fallback
    const raw = sessionStorage.getItem(this.RETRY_KEY);
    if (!raw) return null;

    try {
      const parsed = JSON.parse(raw);

      const isExpired =
        Date.now() - parsed.createdAt > this.RETRY_TTL_MS;

      if (isExpired) {
        this.clearLastLoginRequest();
        return null;
      }

      this.lastLoginRequestMemory = parsed.payload;
      return parsed.payload as LoginRequest;

    } catch {
      this.clearLastLoginRequest();
      return null;
    }
  }

  clearLastLoginRequest() {
    this.lastLoginRequestMemory = null;
    sessionStorage.removeItem(this.RETRY_KEY);
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