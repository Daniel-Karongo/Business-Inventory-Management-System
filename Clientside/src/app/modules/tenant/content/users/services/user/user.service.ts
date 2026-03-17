import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { map, Observable } from 'rxjs';
import { User, UserImage, UserImageAudit } from '../../models/user.model';
import { environment } from '../../../../../../../environments/environment';
import { AuthService } from '../../../../../auth/services/auth.service';
import { BulkRequest, BulkResult } from '../../../../../../shared/models/bulk-import.model';
import { UploadImagePayload } from '../../../../../../core/models/file-upload.model';

@Injectable({ providedIn: 'root' })
export class UserService {

  private base = `${environment.apiUrl}/users`;

  constructor(
    private http: HttpClient,
    private authService: AuthService
  ) { }

  /* ============================================================
     LIST USERS
  ============================================================ */

  list(
    page = 0,
    size = 25,
    filter: Record<string, string | number | boolean | null> = {},
    sortField?: string | null,
    sortDir: 'asc' | 'desc' = 'asc'
  ): Observable<{ data: User[]; total: number }> {

    let params = new HttpParams()
      .set('page', page)
      .set('size', size);
    
      if (sortField) {
      params = params.set('sort', `${sortField},${sortDir}`);
    }

    Object.keys(filter).forEach(key => {
      const value = filter[key];
      if (value !== null && value !== undefined && value !== '') {
        params = params.set(key, String(value));
      }
    });

    const me = this.authService.getSnapshot();
    const canSeeDeleted =
      !!me && ['SUPERUSER', 'ADMIN', 'MANAGER'].includes(me.role);

    if (!canSeeDeleted) {
      params = params.set('deleted', 'false');
    }

    return this.http.get<any>(`${this.base}/all`, { params }).pipe(
      map(res => ({
        data: res.content ?? [],
        total: res.totalElements ?? 0
      }))
    );
  }

  /* ============================================================
     GET SINGLE USER
  ============================================================ */

  get(identifier: string, deleted = false): Observable<User> {

    const params = new HttpParams()
      .set('deleted', deleted);

    return this.http.get<User>(
      `${this.base}/user/${identifier}`,
      { params }
    );
  }

  /* ============================================================
     CREATE USER
  ============================================================ */

  create(payload: FormData) {
    return this.http.post<User>(
      `${this.base}/register`,
      payload
    );
  }

  /* ============================================================
     BULK IMPORT
  ============================================================ */

  bulkImport(
    request: BulkRequest<any>
  ): Observable<BulkResult<User>> {

    return this.http.post<BulkResult<User>>(
      `${this.base}/import`,
      request
    );
  }

  /* ============================================================
     UPDATE USER
  ============================================================ */

  update(identifier: string, payload: Partial<User>): Observable<User> {

    return this.http.patch<User>(
      `${this.base}/${identifier}`,
      payload
    );
  }

  /* ============================================================
     SOFT DELETE
  ============================================================ */

  softDelete(id: string, reason: string | null) {

    return this.http.delete(
      `${this.base}/soft/${id}`,
      {
        body: reason ?? ''
      }
    );
  }

  softDeleteBulk(ids: string[], reason: string | null) {

    return this.http.delete(
      `${this.base}/soft/bulk`,
      {
        body: {
          ids,
          reason: reason ?? ''
        }
      }
    );
  }

  /* ============================================================
     RESTORE
  ============================================================ */

  restore(id: string, reason: string | null) {

    return this.http.patch(
      `${this.base}/restore/${id}`,
      reason ?? ''
    );
  }

  restoreBulk(ids: string[], reason: string | null) {

    return this.http.patch(
      `${this.base}/restore/bulk`,
      {
        ids,
        reason: reason ?? ''
      }
    );
  }

  /* ============================================================
     HARD DELETE
  ============================================================ */

  hardDelete(id: string, reason: string | null) {

    return this.http.delete(
      `${this.base}/hard/${id}`,
    );
  }

  hardDeleteBulk(ids: string[], reason: string | null) {

    return this.http.delete(
      `${this.base}/hard/bulk`,
      {
        body: ids
      }
    );
  }

  /* ============================================================
     USER IMAGES
  ============================================================ */

  uploadImages(
    identifier: string,
    files: UploadImagePayload[],
    deleteOldImages = false
  ) {

    const fd = new FormData();

    files.forEach((f, i) => {

      fd.append(`userImagesFiles[${i}].file`, f.file);
      fd.append(`userImagesFiles[${i}].description`, f.description ?? '');
    });

    fd.append('deleteOldImages', String(deleteOldImages));

    return this.http.patch(
      `${this.base}/${identifier}/images`,
      fd
    );
  }

  listImages(
    identifier: string,
    deleted?: boolean
  ): Observable<UserImage[]> {

    let params = new HttpParams();

    if (deleted !== undefined) {
      params = params.set('deleted', String(deleted));
    }

    return this.http.get<UserImage[]>(
      `${this.base}/images/all/${identifier}`,
      { params }
    );
  }

  deleteAllImages(identifier: string) {

    return this.http.delete(
      `${this.base}/images/all/${identifier}/soft`
    );
  }

  restoreImage(identifier: string, filename: string) {

    return this.http.patch(
      `${this.base}/images/${identifier}/${filename}/restore`,
      {},
      { responseType: 'text' }
    );
  }

  softDeleteImage(identifier: string, filename: string) {

    return this.http.delete(
      `${this.base}/images/${identifier}/${filename}/soft`,
      { responseType: 'text' }
    );
  }

  restoreAllImages(identifier: string) {

    return this.http.patch(
      `${this.base}/images/all/${identifier}/restore`,
      {}
    );
  }

  hardDeleteImage(identifier: string, filename: string) {

    return this.http.delete(
      `${this.base}/images/${identifier}/${filename}/hard`,
      { responseType: 'text' }
    );
  }

  hardDeleteAllImages(identifier: string) {

    return this.http.delete(
      `${this.base}/images/all/${identifier}/hard`
    );
  }

  /* ============================================================
     USER AUDITS
  ============================================================ */

  auditsForUser(identifier: string) {

    return this.http.get<any[]>(
      `${this.base}/audits/${identifier}/target`
    );
  }

  auditsDoneByUser(identifier: string) {

    return this.http.get<any[]>(
      `${this.base}/audits/${identifier}/doer`
    );
  }

  imageAuditTarget(identifier: string) {

    return this.http.get<UserImageAudit[]>(
      `${this.base}/images/audits/${identifier}/receiver`
    );
  }

  imageAuditDoer(identifier: string) {

    return this.http.get<any[]>(
      `${this.base}/images/audits/${identifier}/doer`
    );
  }

  /* ============================================================
     USER ROLLCALLS
  ============================================================ */

  getUserRollcalls(userId: string) {

    return this.http.get<any[]>(
      `${environment.apiUrl}/rollcall/user/${userId}`
    );
  }

  getUserImageBlob(
    userId: string,
    fileName: string,
    deleted = false
  ) {

    return this.http.get(
      `${this.base}/images/${userId}/${encodeURIComponent(fileName)}?deleted=${deleted}`,
      { responseType: 'blob' }
    );
  }
}