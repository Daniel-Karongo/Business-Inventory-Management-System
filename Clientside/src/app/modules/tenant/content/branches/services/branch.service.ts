import { Injectable, inject } from '@angular/core';
import {
  HttpClient,
  HttpParams
} from '@angular/common/http';

import { map, Observable } from 'rxjs';

import { environment } from '../../../../../../environments/environment';

import {
  BranchAttendanceSettingsDTO,
  BranchAuditDTO,
  BranchAuditTableState,
  BranchDeletionMode,
  BranchDetailsDTO,
  BranchEmailSettingsDTO,
  BranchEmailSettingsResponseDTO,
  BranchFormDTO,
  BranchListItemDTO,
  BranchMpesaSettingsDTO,
  BranchMpesaSettingsResponseDTO,
  BranchNotificationSettingsDTO,
  BranchSecuritySettingsDTO,
  BranchSmsSettingsDTO,
  BranchSmsSettingsResponseDTO,
  BranchTableState
} from '../models/branch.model';

import { ApiResponse } from '../../../../../core/models/api-response.model';

import { PageWrapper } from '../../../../../core/models/page-wrapper.model';

import {
  BulkRequest,
  BulkResult
} from '../../../../../shared/models/bulk-import.model';

@Injectable({
  providedIn: 'root'
})
export class BranchService {

  private http = inject(HttpClient);

  private readonly base =
    `${environment.apiUrl}${environment.endpoints.branches.base}`;

  /* =========================================================
     CREATE
  ========================================================= */

  create(
    dto: BranchFormDTO
  ): Observable<ApiResponse<BranchDetailsDTO>> {

    return this.http.post<ApiResponse<BranchDetailsDTO>>(
      this.base,
      dto
    );
  }

  /* =========================================================
     BULK IMPORT
  ========================================================= */

  bulkImport(
    request: BulkRequest<any>
  ): Observable<BulkResult<BranchDetailsDTO>> {

    return this.http.post<BulkResult<BranchDetailsDTO>>(
      `${this.base}/import`,
      request
    );
  }

  /* =========================================================
     LIST
  ========================================================= */

  getAll(
    state: Partial<BranchTableState>
  ): Observable<PageWrapper<BranchListItemDTO>> {

    let params = new HttpParams();

    if (state.search?.trim()) {
      params = params.set(
        'search',
        state.search.trim()
      );
    }

    params = params.set(
      'deleted',
      String(!!state.includeDeleted)
    );

    params = params.set(
      'page',
      String(state.page ?? 0)
    );

    params = params.set(
      'size',
      String(state.size ?? 20)
    );

    params = params.set(
      'sortBy',
      state.sortBy ?? 'createdAt'
    );

    params = params.set(
      'sortDirection',
      state.sortDirection ?? 'desc'
    );

    return this.http.get<PageWrapper<BranchListItemDTO>>(
      this.base,
      { params }
    );
  }

  /* =========================================================
   LEGACY COMPATIBILITY
   TEMPORARY MIGRATION BRIDGE
========================================================= */

  getAllLegacy(): Observable<BranchListItemDTO[]> {

    return this.getAll({
      page: 0,
      size: 1000
    }).pipe(
      map(response => response.content)
    );
  }

  /* =========================================================
     DETAILS
  ========================================================= */

  getById(
    id: string
  ): Observable<BranchDetailsDTO> {

    return this.http.get<BranchDetailsDTO>(
      `${this.base}/${id}`
    );
  }

  /* =========================================================
   ATTENDANCE SETTINGS
========================================================= */

  updateAttendanceSettings(
    id: string,
    dto: BranchAttendanceSettingsDTO
  ): Observable<BranchDetailsDTO> {

    return this.http.patch<BranchDetailsDTO>(
      `${this.base}/${id}/attendance-settings`,
      dto
    );
  }

  /* =========================================================
     SECURITY SETTINGS
  ========================================================= */

  updateSecuritySettings(
    id: string,
    dto: BranchSecuritySettingsDTO
  ): Observable<BranchDetailsDTO> {

    return this.http.patch<BranchDetailsDTO>(
      `${this.base}/${id}/security-settings`,
      dto
    );
  }

  /* =========================================================
     NOTIFICATION SETTINGS
  ========================================================= */

  getNotificationSettings(
    branchId: string
  ): Observable<BranchNotificationSettingsDTO> {

    return this.http.get<BranchNotificationSettingsDTO>(
      `${environment.apiUrl}/settings/notifications/branch/${branchId}`
    );
  }

  updateNotificationSettings(
    branchId: string,
    dto: BranchNotificationSettingsDTO
  ): Observable<BranchNotificationSettingsDTO> {

    return this.http.put<BranchNotificationSettingsDTO>(
      `${environment.apiUrl}/settings/notifications/branch/${branchId}`,
      dto
    );
  }

  /* =========================================================
     EMAIL SETTINGS
  ========================================================= */

  getEmailSettings(
    branchId: string
  ): Observable<BranchEmailSettingsResponseDTO> {

    return this.http.get<BranchEmailSettingsResponseDTO>(
      `${environment.apiUrl}/settings/email/branch/${branchId}`
    );
  }

  updateEmailSettings(
    branchId: string,
    dto: BranchEmailSettingsDTO
  ): Observable<BranchEmailSettingsResponseDTO> {

    return this.http.put<BranchEmailSettingsResponseDTO>(
      `${environment.apiUrl}/settings/email/branch/${branchId}`,
      this.sanitizeEmailSettingsPayload(dto)
    );
  }

  /* =========================================================
     SMS SETTINGS
  ========================================================= */

  getSmsSettings(
    branchId: string
  ): Observable<BranchSmsSettingsResponseDTO> {

    return this.http.get<BranchSmsSettingsResponseDTO>(
      `${environment.apiUrl}/settings/sms/branch/${branchId}`
    );
  }

  updateSmsSettings(
    branchId: string,
    dto: BranchSmsSettingsDTO
  ): Observable<BranchSmsSettingsResponseDTO> {

    return this.http.put<BranchSmsSettingsResponseDTO>(
      `${environment.apiUrl}/settings/sms/branch/${branchId}`,
      this.sanitizeSmsSettingsPayload(dto)
    );
  }

  /* =========================================================
     MPESA SETTINGS
  ========================================================= */

  getMpesaSettings(
    branchId: string
  ): Observable<BranchMpesaSettingsResponseDTO> {

    return this.http.get<BranchMpesaSettingsResponseDTO>(
      `${environment.apiUrl}/settings/mpesa/branch/${branchId}`
    );
  }

  updateMpesaSettings(
    branchId: string,
    dto: BranchMpesaSettingsDTO
  ): Observable<BranchMpesaSettingsResponseDTO> {

    return this.http.put<BranchMpesaSettingsResponseDTO>(
      `${environment.apiUrl}/settings/mpesa/branch/${branchId}`,
      this.sanitizeMpesaSettingsPayload(dto)
    );
  }

  /* =========================================================
   AUDITS
========================================================= */

  getAudits(
    branchId: string,
    state: Partial<BranchAuditTableState>
  ): Observable<PageWrapper<BranchAuditDTO>> {

    let params = new HttpParams();

    params = params.set(
      'page',
      String(state.page ?? 0)
    );

    params = params.set(
      'size',
      String(state.size ?? 20)
    );

    if (state.search?.trim()) {

      params = params.set(
        'search',
        state.search.trim()
      );
    }

    return this.http.get<PageWrapper<BranchAuditDTO>>(
      `${this.base}/${branchId}/audits`,
      { params }
    );
  }

  /* =========================================================
     UPDATE
  ========================================================= */

  update(
    id: string,
    dto: BranchFormDTO
  ): Observable<BranchDetailsDTO> {

    return this.http.patch<BranchDetailsDTO>(
      `${this.base}/${id}`,
      dto
    );
  }

  /* =========================================================
     DELETE
  ========================================================= */

  delete(
    id: string,
    mode: BranchDeletionMode
  ): Observable<ApiResponse> {
    return this.http.delete<ApiResponse>(
      `${this.base}/${id}`,
      {
        params: {
          mode
        }
      }
    );
  }

  deleteBulk(
    ids: string[],
    mode: BranchDeletionMode
  ): Observable<ApiResponse> {
    return this.http.delete<ApiResponse>(
      `${this.base}/bulk`,
      {
        body: ids,
        params: {
          mode
        }
      }
    );
  }



  /* =========================================================
     RESTORE
  ========================================================= */

  restore(
    id: string
  ): Observable<ApiResponse> {

    return this.http.patch<ApiResponse>(
      `${this.base}/restore/${id}`,
      {}
    );
  }

  /* =========================================================
     BULK RESTORE
  ========================================================= */

  restoreBulk(
    ids: string[]
  ): Observable<ApiResponse> {

    return this.http.patch<ApiResponse>(
      `${this.base}/restore/bulk`,
      ids
    );
  }

  /* =========================================================
     Helpers
  ========================================================= */

  sanitizeEmailSettingsPayload(
    dto: BranchEmailSettingsDTO
  ): BranchEmailSettingsDTO {

    const payload = { ...dto };

    if (
      !payload.password ||
      payload.password === '********'
    ) {
      delete payload.password;
    }

    return payload;
  }

  sanitizeSmsSettingsPayload(
    dto: BranchSmsSettingsDTO
  ): BranchSmsSettingsDTO {

    const payload = { ...dto };

    if (
      !payload.apiKey ||
      payload.apiKey === '********'
    ) {
      delete payload.apiKey;
    }

    return payload;
  }

  sanitizeMpesaSettingsPayload(
    dto: BranchMpesaSettingsDTO
  ): BranchMpesaSettingsDTO {

    const payload = { ...dto };

    if (
      !payload.consumerKey ||
      payload.consumerKey === '********'
    ) {
      delete payload.consumerKey;
    }

    if (
      !payload.consumerSecret ||
      payload.consumerSecret === '********'
    ) {
      delete payload.consumerSecret;
    }

    if (
      !payload.passkey ||
      payload.passkey === '********'
    ) {
      delete payload.passkey;
    }

    if (
      !payload.securityCredential ||
      payload.securityCredential === '********'
    ) {
      delete payload.securityCredential;
    }

    return payload;
  }
}