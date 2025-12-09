import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { map, Observable } from 'rxjs';
import { User } from '../../models/user.model';
import { environment } from '../../../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class UserService {
  private base = environment.endpoints.users.base;

  constructor(private http: HttpClient) {}

  /* ============================================================
     LIST USERS
  ============================================================ */
  list(
    page = 0,
    size = 25,
    filter: Record<string, string | number | boolean | null> = {}
  ): Observable<{ data: User[]; total: number }> {

    let params = new HttpParams();

    // Optional filters (search, branch, etc)
    Object.keys(filter).forEach(key => {
      if (key === 'deleted') return;

      const value = filter[key];
      if (value !== null && value !== undefined && value !== '') {
        params = params.set(key, String(value));
      }
    });

    // ---- FIXED ROLE LOADING ----
    const roleData = localStorage.getItem("auth_roles");
    let userRole = '';

    try {
      const arr = JSON.parse(roleData || '[]');
      userRole = Array.isArray(arr) ? arr[0] : '';
    } catch {
      userRole = '';
    }

    const canSeeDeleted = ['SUPERUSER', 'ADMIN', 'MANAGER'].includes(userRole);

    // If NOT privileged → force deleted=false
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

  /* ============================================================
     UPDATE USER (PATCH)
     Backend: PATCH /api/users/{identifier}
  ============================================================ */
  update(identifier: string, payload: Partial<User>): Observable<User> {
    const url = `${environment.apiUrl}${environment.endpoints.users.update(identifier)}`;
    return this.http.patch<User>(url, payload);
  }

  /* ============================================================
     DELETE / RESTORE USERS
  ============================================================ */
  softDelete(id: string, reason: string | null) {
    return this.http.delete(
      `${environment.apiUrl}${environment.endpoints.users.softDelete(id)}`,
      {
        body: reason ?? '' // send plain string
      }
    );
  }

  softDeleteBulk(ids: string[]) {
    const url = `${environment.apiUrl}${environment.endpoints.users.softDeleteBulk}`;
    return this.http.delete(url, { body: ids });
  }

  restore(id: string, reason: string | null) {
    return this.http.patch(
      `${environment.apiUrl}${environment.endpoints.users.restore(id)}`,
      reason ?? ''   // send plain string
    );
  }

  restoreBulk(ids: string[]) {
    const url = `${environment.apiUrl}${environment.endpoints.users.restoreBulk}`;
    return this.http.patch(url, ids);
  }

  hardDelete(id: string) {
    const url = `${environment.apiUrl}${environment.endpoints.users.hardDelete(id)}`;
    return this.http.delete(url);
  }

  hardDeleteBulk(ids: string[]) {
    const url = `${environment.apiUrl}${environment.endpoints.users.hardDeleteBulk}`;
    return this.http.delete(url, { body: ids });
  }

  /* ============================================================
     USER IMAGES
  ============================================================ */

  uploadImage(identifier: string, file: File) {
    const fd = new FormData();
    fd.append('userImagesFiles', file);

    const url = `${environment.apiUrl}${environment.endpoints.users.updateImages(identifier)}`;
    return this.http.patch(url, fd);
  }

  listImages(identifier: string, deleted = false): Observable<string[]> {
    const url = `${environment.apiUrl}${environment.endpoints.users.images.forUser(identifier, deleted)}`;
    return this.http.get<string[]>(url);
  }

  deleteAllImages(identifier: string) {
    const url = `${environment.apiUrl}${environment.endpoints.users.images.softDeleteAll(identifier)}`;
    return this.http.delete(url);
  }

  restoreImage(identifier: string, filename: string) {
    const url = `${environment.apiUrl}${environment.endpoints.users.images.restore(identifier, filename)}`;
    return this.http.patch(url, {});
  }

  restoreAllImages(identifier: string) {
    const url = `${environment.apiUrl}${environment.endpoints.users.images.restoreAll(identifier)}`;
    return this.http.patch(url, {});
  }

  hardDeleteImage(identifier: string, filename: string) {
    const url = `${environment.apiUrl}${environment.endpoints.users.images.hardDelete(identifier, filename)}`;
    return this.http.delete(url);
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
    return this.http.get<any[]>(url);
  }

  imageAuditDoer(identifier: string) {
    const url = `${environment.apiUrl}${environment.endpoints.users.audits.imageDoer(identifier)}`;
    return this.http.get<any[]>(url);
  }
}