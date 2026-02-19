import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { map, Observable } from 'rxjs';
import { User, UserImage, UserImageAudit } from '../../models/user.model';
import { environment } from '../../../../../environments/environment';
import { UploadImagePayload } from '../../../../core/models/file-upload.model';
import { BulkRequest, BulkResult } from '../../../../shared/models/bulk-import.model';
import { AuthService } from '../../../auth/services/auth.service';

@Injectable({ providedIn: 'root' })
export class UserService {
  private base = environment.endpoints.users.base;

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
    filter: Record<string, string | number | boolean | null> = {}
  ): Observable<{ data: User[]; total: number }> {

    let params = new HttpParams();

    Object.keys(filter).forEach(key => {
      if (key === 'deleted') return;

      const value = filter[key];
      if (value !== null && value !== undefined && value !== '') {
        params = params.set(key, String(value));
      }
    });

    const me = this.authService.getSnapshot();
    const canSeeDeleted =
      !!me && ['SUPERUSER', 'ADMIN', 'MANAGER'].includes(me.role);

    const url = `${environment.apiUrl}${environment.endpoints.users.getAll(
      canSeeDeleted ? undefined : false
    )}`;

    return this.http.get<User[]>(url, { params }).pipe(
      map(users => {
        const start = page * size;
        const end = start + size;

        return {
          data: users.slice(start, end),
          total: users.length
        };
      })
    );
  }

  /* ============================================================
     GET SINGLE USER
     Backend: GET /api/users/user/{identifier}?deleted=x
  ============================================================ */
  get(identifier: string, deleted = false): Observable<User> {
    const url = `${environment.apiUrl}${environment.endpoints.users.get(identifier, deleted)}`;
    return this.http.get<User>(url);
  }

  /* ============================================================
     CREATE USER (multipart form-data)
     Backend: POST /api/users/register
  ============================================================ */
  create(payload: FormData) {
    return this.http.post<User>(
      `${environment.apiUrl}${environment.endpoints.users.register}`,
      payload
    );
  }

  bulkImport(
    request: BulkRequest<any>
  ): Observable<BulkResult<User>> {
    return this.http.post<BulkResult<User>>(
      `${environment.apiUrl}${environment.endpoints.users.base}/import`,
      request
    );
  }


  /* ============================================================
     UPDATE USER (PATCH)
     Backend: PATCH /api/users/{identifier}
  ============================================================ */
  update(identifier: string, payload: Partial<User>): Observable<User> {
    const url = `${environment.apiUrl}${environment.endpoints.users.update(identifier)}`;
    return this.http.patch<User>(url, payload);
  }

  softDelete(id: string, reason: string | null) {
    return this.http.delete(`${this.base}/${id}`, {
      params: { soft: true },
      body: reason ?? ''
    });
  }

  restore(id: string, reason: string | null) {
    return this.http.patch(`${this.base}/${id}/restore`, reason ?? '');
  }

  hardDelete(id: string, reason: string | null) {
    return this.http.delete<void>(
      `${this.base}/${id}`,
      {
        params: { soft: 'false' },
        body: reason ?? ''
      }
    );
  }

  softDeleteBulk(ids: string[], reason: string | null) {
    return this.http.post(`${this.base}/bulk/soft-delete`, {
      ids,
      reason: reason ?? ''
    });
  }

  restoreBulk(ids: string[], reason: string | null) {
    return this.http.post(`${this.base}/bulk/restore`, {
      ids,
      reason: reason ?? ''
    });
  }

  hardDeleteBulk(ids: string[], reason: string | null) {
    return this.http.post(`${this.base}/bulk/hard-delete`, {
      ids,
      reason: reason ?? ''
    });
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

    const url = `${environment.apiUrl}${environment.endpoints.users.updateImages(identifier)}`;
    return this.http.patch(url, fd);
  }


  listImages(
    identifier: string,
    deleted?: boolean
  ): Observable<UserImage[]> {

    let params = new HttpParams();

    if (deleted !== undefined) {
      params = params.set('deleted', String(deleted));
    }

    console.log(params)

    const url =
      `${environment.apiUrl}${environment.endpoints.users.images.forUser(identifier)}`;

    return this.http.get<UserImage[]>(url, { params });
  }

  deleteAllImages(identifier: string) {
    const url = `${environment.apiUrl}${environment.endpoints.users.images.softDeleteAll(identifier)}`;
    return this.http.delete(url);
  }

  restoreImage(identifier: string, filename: string) {
    const url =
      `${environment.apiUrl}${environment.endpoints.users.images.restore(identifier, filename)}`;

    return this.http.patch(url, {}, {
      responseType: 'text'   // ✅ REQUIRED
    });
  }

  softDeleteImage(identifier: string, filename: string) {
    const url =
      `${environment.apiUrl}${environment.endpoints.users.images.softDelete(identifier, filename)}`;

    return this.http.delete(url, {
      responseType: 'text'   // ✅ REQUIRED
    });
  }

  restoreAllImages(identifier: string) {
    const url = `${environment.apiUrl}${environment.endpoints.users.images.restoreAll(identifier)}`;
    return this.http.patch(url, {});
  }

  hardDeleteImage(identifier: string, filename: string) {
    const url =
      `${environment.apiUrl}${environment.endpoints.users.images.hardDelete(identifier, filename)}`;

    return this.http.delete(url, {
      responseType: 'text'   // ✅ REQUIRED
    });
  }

  hardDeleteAllImages(identifier: string) {
    const url = `${environment.apiUrl}${environment.endpoints.users.images.hardDeleteAll(identifier)}`;
    return this.http.delete(url);
  }

  /* ============================================================
     USER AUDITS
  ============================================================ */
  auditsForUser(identifier: string) {
    const url = `${environment.apiUrl}${environment.endpoints.users.audits.userTarget(identifier)}`;
    return this.http.get<any[]>(url);
  }

  auditsDoneByUser(identifier: string) {
    const url = `${environment.apiUrl}${environment.endpoints.users.audits.userDoer(identifier)}`;
    return this.http.get<any[]>(url);
  }

  imageAuditTarget(identifier: string) {
    const url = `${environment.apiUrl}${environment.endpoints.users.audits.imageTarget(identifier)}`;
    return this.http.get<UserImageAudit[]>(url);
  }

  imageAuditDoer(identifier: string) {
    const url = `${environment.apiUrl}${environment.endpoints.users.audits.imageDoer(identifier)}`;
    return this.http.get<any[]>(url);
  }

  /* ============================================================
     USER ROLLCALLS
  ============================================================ */

  getUserRollcalls(userId: string) {
    return this.http.get<any[]>(
      `${environment.apiUrl}/rollcall/user/${userId}`
    );
  }

  getUserImageBlob(userId: string, fileName: string, deleted = false) {
    const url =
      `${environment.apiUrl}/users/images/${userId}/${encodeURIComponent(fileName)}?deleted=${deleted}`;

    return this.http.get(url, {
      responseType: 'blob'
    });
  }
}