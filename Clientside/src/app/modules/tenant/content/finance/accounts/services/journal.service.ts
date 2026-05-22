import { Injectable, inject } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';

import { environment } from '../../../../../../../environments/environment';

import {
  Journal,
  JournalReversalRequest
} from '../models/journal.models';

import {
  PageRequest,
  PageResponse
} from '../models/pagination.models';
import { BranchContextService } from '../../../../../../core/services/branch-context.service';

@Injectable({
  providedIn: 'root'
})
export class JournalService {

  private readonly http = inject(HttpClient);

  private readonly branchContext =
    inject(BranchContextService);

  private readonly baseUrl =
    `${environment.apiUrl}/accounting/journals`;

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
  ): Observable<PageResponse<Journal>> {

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

    return this.http.get<PageResponse<Journal>>(
      this.baseUrl,
      { params }
    );
  }

  get(id: string): Observable<Journal> {
    return this.http.get<Journal>(
      `${this.baseUrl}/${id}`
    );
  }

  reverse(
    id: string,
    payload: JournalReversalRequest
  ): Observable<void> {

    return this.http.post<void>(
      `${this.baseUrl}/${id}/reverse`,
      payload
    );
  }
}