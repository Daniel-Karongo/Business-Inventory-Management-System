import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../environments/environment';
import { Observable } from 'rxjs';

export interface DeviceDTO {
  id: string;
  branchId: string | null;
  deviceName: string;
  fingerprint: string;
  approved: boolean;
  firstSeenAt: string;
  lastSeenAt: string;
  usedByUserIds: string[];
}

@Injectable({ providedIn: 'root' })
export class DeviceApiService {

  private http = inject(HttpClient);
  private base = `${environment.apiUrl}/devices`;

  list(branchId: string | null): Observable<DeviceDTO[]> {
    return this.http.get<DeviceDTO[]>(`${this.base}/branch/${branchId}`);
  }

  approve(id: string) {
    return this.http.post(`${this.base}/${id}/approve`, {});
  }

  reject(id: string) {
    return this.http.post(`${this.base}/${id}/reject`, {});
  }

  rename(id: string, name: string) {
    return this.http.patch(`${this.base}/${id}/rename?name=${name}`, {});
  }
}