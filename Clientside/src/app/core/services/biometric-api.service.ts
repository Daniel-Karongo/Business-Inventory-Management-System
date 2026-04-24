import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../environments/environment';
import { Observable } from 'rxjs';

export interface UserBiometricDTO {
  id: string;
  deviceName: string;
  deviceId: string;
}

@Injectable({ providedIn: 'root' })
export class BiometricApiService {

  private http = inject(HttpClient);
  private base = `${environment.apiUrl}/biometrics`;

  list(): Observable<UserBiometricDTO[]> {
    return this.http.get<UserBiometricDTO[]>(this.base, {
      withCredentials: true
    });
  }

  delete(id: string) {
    return this.http.delete(`${this.base}/${id}`, {
      withCredentials: true
    });
  }

  adminListForUser(userId: string): Observable<UserBiometricDTO[]> {

    return this.http.get<UserBiometricDTO[]>(
      `${environment.apiUrl}/admin/biometrics/user/${userId}`,
      {
        withCredentials: true
      }
    );

  }

  adminDelete(id: string) {

    return this.http.delete(
      `${environment.apiUrl}/admin/biometrics/${id}`,
      {
        withCredentials: true
      }
    );

  }
}