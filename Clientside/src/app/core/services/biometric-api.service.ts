import { Injectable, inject } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { map, Observable } from 'rxjs';
import { environment } from '../../../environments/environment';

export interface UserBiometricDTO {
  id: string;
  deviceName: string;
  deviceId: string;
  browserName?: string;
  osName?: string;
  platform?: string;
  firstSeenAt?: string;
  lastSeenAt?: string;
  trustedDeviceStatus?:
  'APPROVED'
  | 'PENDING'
  | 'REJECTED';
}

export interface BiometricStatsDTO {
  activeCredentials: number;
  uniqueUsers: number;
  uniqueDevices: number;
}

@Injectable({ providedIn: 'root' })
export class BiometricApiService {

  private http = inject(HttpClient);
  private base = `${environment.apiUrl}/biometrics`;

  list(): Observable<UserBiometricDTO[]> {
    return this.http.get<UserBiometricDTO[]>(
      this.base,
      { withCredentials: true }
    );
  }

  hasCredentialForCurrentDevice(
    deviceId: string
  ): Observable<boolean> {

    return this.list().pipe(
      map(list =>
        list.some(
          b => b.deviceId === deviceId
        )
      )
    );

  }
  
  rename(
    id: string,
    name: string
  ): Observable<void> {
    return this.http.put<void>(
      `${this.base}/${id}/rename`,
      {},
      {
        params: { name },
        withCredentials: true
      }
    );
  }

  delete(
    id: string,
    hard = false
  ): Observable<void> {
    return this.http.delete<void>(
      `${this.base}/${id}`,
      {
        params: { hard },
        withCredentials: true
      }
    );
  }

  adminListForUser(
    userId: string
  ): Observable<UserBiometricDTO[]> {
    return this.http.get<UserBiometricDTO[]>(
      `${environment.apiUrl}/admin/biometrics/user/${userId}`,
      { withCredentials: true }
    );
  }

  stats(): Observable<BiometricStatsDTO> {
    return this.http.get<BiometricStatsDTO>(
      `${environment.apiUrl}/admin/biometrics/stats`,
      { withCredentials: true }
    );
  }

  adminDelete(
    id: string,
    hard = false
  ): Observable<void> {
    return this.http.delete<void>(
      `${environment.apiUrl}/admin/biometrics/${id}`,
      {
        params: { hard },
        withCredentials: true
      }
    );
  }
}