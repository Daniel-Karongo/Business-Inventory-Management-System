import { Injectable, inject } from '@angular/core';

import {
  HttpClient,
  HttpParams
} from '@angular/common/http';

import {
  Observable
} from 'rxjs';

import {
  map
} from 'rxjs/operators';

import {
  environment
} from '../../../../../../environments/environment';
import { PlatformUser, PlatformUserAuditPage, PlatformUserPage } from '../models/user.model';


/* =========================================
   API
========================================= */

@Injectable({
  providedIn: 'root'
})
export class PlatformUserApiService {

  private http =
    inject(HttpClient);

  private base =
    `${environment.apiUrl}/platform/users`;


  /* =====================================
     PAGED LIST
  ===================================== */

  list(
    page = 0,
    size = 20
  ): Observable<PlatformUserPage> {

    const params =
      new HttpParams()
        .set('page', String(page))
        .set('size', String(size));

    return this.http.get<PlatformUserPage>(
      this.base,
      { params }
    );

  }


  /* =====================================
     ALL USERS
  ===================================== */

  listAll():
    Observable<PlatformUser[]> {

    return this.list(
      0,
      1000
    ).pipe(
      map(
        res => res.content || []
      )
    );

  }


  /* =====================================
     GET ONE
  ===================================== */

  getById(
    id: string
  ): Observable<PlatformUser> {

    return this.http.get<PlatformUser>(
      `${this.base}/${id}`
    );

  }


  /* =====================================
     CREATE
  ===================================== */

  create(
    payload: any
  ): Observable<PlatformUser> {

    return this.http.post<PlatformUser>(
      this.base,
      payload
    );

  }


  /* =====================================
     UPDATE
  ===================================== */

  update(
    id: string,
    payload: any
  ): Observable<PlatformUser> {

    return this.http.put<PlatformUser>(
      `${this.base}/${id}`,
      payload
    );

  }


  /* =====================================
     LOCK / UNLOCK
  ===================================== */

  lock(
    id: string
  ): Observable<void> {

    return this.http.patch<void>(
      `${this.base}/${id}/lock`,
      {}
    );

  }

  unlock(
    id: string
  ): Observable<void> {

    return this.http.patch<void>(
      `${this.base}/${id}/unlock`,
      {}
    );

  }


  /* =====================================
     DELETE
  ===================================== */

  delete(
    id: string
  ): Observable<void> {

    return this.http.delete<void>(
      `${this.base}/${id}`
    );

  }


  /* =====================================
     AUDIT
  ===================================== */

  audit(
    page = 0,
    size = 20
  ): Observable<PlatformUserAuditPage> {

    const params = new HttpParams()
      .set('page', String(page))
      .set('size', String(size))
      .set('sort', 'timestamp,desc');

    return this.http.get<PlatformUserAuditPage>(
      `${this.base}/audit`,
      { params }
    );
  }
}