import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../environments/environment';
import { Observable } from 'rxjs';

export interface UserBiometricDTO {
  id: string;
  deviceName: string;
  fingerprint: string;
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
}