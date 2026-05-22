import { Injectable, inject } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';

import { environment } from '../../../../../../../environments/environment';

import {
  Account,
  CreateAccountRequest,
  UpdateAccountRequest
} from '../models/account.models';

import {
  PageRequest,
  PageResponse
} from '../models/pagination.models';
import { BranchContextService } from '../../../../../../core/services/branch-context.service';

@Injectable({
  providedIn: 'root'
})
export class AccountsService {

  private readonly http = inject(HttpClient);
  private readonly branchContext = inject(BranchContextService);

  private readonly baseUrl =
    `${environment.apiUrl}/accounts`;

  private readonly adminUrl =
    `${environment.apiUrl}/accounts/admin`;

  private resolveBranchId(
    override?: string
  ): string {

    const branchId =
      override
      ??
      this.branchContext.currentBranch;

    if (!branchId) {
      throw new Error(
        'Branch not selected'
      );
    }

    return branchId;
  }

  list(
    request?: PageRequest
  ): Observable<PageResponse<Account>> {

    let params = new HttpParams()
      .set(
        'branchId',
        this.resolveBranchId()
      );

    if (request?.page !== undefined) {
      params = params.set('page', request.page);
    }

    if (request?.size !== undefined) {
      params = params.set('size', request.size);
    }

    if (request?.sort) {
      params = params.set('sort', request.sort);
    }

    return this.http.get<PageResponse<Account>>(
      this.baseUrl,
      { params }
    );
  }

  get(id: string): Observable<Account> {
    return this.http.get<Account>(
      `${this.baseUrl}/${id}`
    );
  }

  create(
    payload: Omit<CreateAccountRequest, 'branchId'>
  ): Observable<Account> {

    return this.http.post<Account>(
      this.adminUrl,
      {
        ...payload,
        branchId:
          this.resolveBranchId()
      }
    );
  }

  rename(
    id: string,
    payload: UpdateAccountRequest
  ): Observable<Account> {

    return this.http.patch<Account>(
      `${this.adminUrl}/${id}`,
      payload
    );
  }

  deactivate(id: string): Observable<void> {
    return this.http.post<void>(
      `${this.adminUrl}/${id}/deactivate`,
      {}
    );
  }
}