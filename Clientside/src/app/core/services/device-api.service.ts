import { Injectable, inject } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../../environments/environment';

export type DeviceStatus =
  | 'PENDING'
  | 'APPROVED'
  | 'REJECTED';

export interface DeviceDTO {
  id: string;
  branchId: string | null;

  deviceId: string;
  deviceName: string;

  browserName: string;
  osName: string;
  platform: string;

  ipAddress: string;
  userAgent: string;

  attemptedByUserIds: string[];
  pendingAttemptCount: number;

  usedByUserIds?: string[];
  usedByUsernames?: string[];

  status: DeviceStatus;

  firstSeenAt: string;
  lastSeenAt: string;
}

export interface DeviceStatsDTO {
  approvedDevices: number;
  pendingDevices: number;
  rejectedDevices: number;
  devicesInUse: number;
}

export interface DeviceApprovalAuditDTO {
  id: string;
  deviceId: string;
  actedByUserId: string;
  action: string;
  reason: string;
  actedAt: string;
}

@Injectable({
  providedIn: 'root'
})
export class DeviceApiService {

  private http = inject(HttpClient);

  private tenantDevices = environment.endpoints.security.devices;
  private platformDevices = environment.endpoints.security.platformDevices;

  /* =========================
     TENANT DEVICES
  ========================= */

  list(branchId?: string | null): Observable<DeviceDTO[]> {

    const url = branchId
      ? `${environment.apiUrl}${this.tenantDevices.list(branchId)}`
      : `${environment.apiUrl}/admin/devices`;

    return this.http.get<DeviceDTO[]>(url);
  }

  pending(): Observable<DeviceDTO[]> {
    return this.http.get<DeviceDTO[]>(
      `${environment.apiUrl}${this.tenantDevices.pending}`
    );
  }

  approve(
    id: string,
    reason?: string
  ): Observable<void> {

    let params = new HttpParams();

    if (reason?.trim()) {
      params = params.set('reason', reason.trim());
    }

    return this.http.patch<void>(
      `${environment.apiUrl}${this.tenantDevices.approve(id)}`,
      {},
      { params }
    );
  }

  reject(
    id: string,
    reason?: string
  ): Observable<void> {

    let params = new HttpParams();

    if (reason?.trim()) {
      params = params.set('reason', reason.trim());
    }

    return this.http.patch<void>(
      `${environment.apiUrl}${this.tenantDevices.reject(id)}`,
      {},
      { params }
    );
  }

  rename(
    id: string,
    name: string
  ): Observable<void> {

    const params = new HttpParams()
      .set('name', name);

    return this.http.patch<void>(
      `${environment.apiUrl}${this.tenantDevices.rename(id)}`,
      {},
      { params }
    );
  }

  stats(): Observable<DeviceStatsDTO> {
    return this.http.get<DeviceStatsDTO>(
      `${environment.apiUrl}${this.tenantDevices.stats}`
    );
  }

  audit(id: string): Observable<DeviceApprovalAuditDTO[]> {
    return this.http.get<DeviceApprovalAuditDTO[]>(
      `${environment.apiUrl}${this.tenantDevices.audit(id)}`
    );
  }

  /* =========================
     PLATFORM DEVICES
  ========================= */

  platformList(): Observable<DeviceDTO[]> {
    return this.http.get<DeviceDTO[]>(
      `${environment.apiUrl}${this.platformDevices.list}`
    );
  }

  platformStats() {
    return this.http.get<any>(
      `${environment.apiUrl}/platform/devices/stats`
    );
  }

  platformApprove(
    id: string,
    reason?: string
  ): Observable<void> {

    let params = new HttpParams();

    if (reason?.trim()) {
      params = params.set('reason', reason.trim());
    }

    return this.http.patch<void>(
      `${environment.apiUrl}${this.platformDevices.approve(id)}`,
      {},
      { params }
    );
  }

  platformReject(
    id: string,
    reason?: string
  ): Observable<void> {

    let params = new HttpParams();

    if (reason?.trim()) {
      params = params.set('reason', reason.trim());
    }

    return this.http.patch<void>(
      `${environment.apiUrl}${this.platformDevices.reject(id)}`,
      {},
      { params }
    );
  }

  platformRename(
    id: string,
    name: string
  ): Observable<void> {

    const params =
      new HttpParams()
        .set('name', name);

    return this.http.patch<void>(
      `${environment.apiUrl}${this.platformDevices.rename(id)}`,
      {},
      { params }
    );

  }

  platformAudit(
    id: string
  ): Observable<DeviceApprovalAuditDTO[]> {

    return this.http.get<DeviceApprovalAuditDTO[]>(
      `${environment.apiUrl}${this.platformDevices.audit(id)}`
    );
  }

  platformAttempts(id: string) {
    return this.http.get<any[]>(
      `${environment.apiUrl}/platform/devices/${id}/attempts`
    );
  }
}