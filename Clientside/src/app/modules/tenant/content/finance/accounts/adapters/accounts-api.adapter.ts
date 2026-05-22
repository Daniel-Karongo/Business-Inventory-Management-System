import { Injectable, inject } from '@angular/core';
import {
    HttpClient,
    HttpParams
} from '@angular/common/http';

import { map, Observable } from 'rxjs';

import { environment } from '../../../../../../../environments/environment';

import {
    Account,
    CreateAccountRequest,
    UpdateAccountRequest
} from '../models/account.models';

import { TableQuery } from '../models/state/table-query.model';

import { BranchContextService } from '../../../../../../core/services/branch-context.service';
import { PageWrapper } from '../../../../../../core/models/page-wrapper.model';

@Injectable({
    providedIn: 'root'
})
export class AccountsApiAdapter {

    private readonly http =
        inject(HttpClient);

    private readonly branchContext =
        inject(BranchContextService);

    private readonly baseUrl =
        `${environment.apiUrl}/accounts`;

    private readonly adminUrl =
        `${environment.apiUrl}/accounts/admin`;

    private resolveBranchId(): string {

        const branchId =
            this.branchContext.currentBranch;

        if (!branchId) {
            throw new Error(
                'Branch not selected'
            );
        }

        return branchId;
    }

    list(
        query: TableQuery
    ): Observable<PageWrapper<Account>> {

        let params =
            new HttpParams()
                .set(
                    'branchId',
                    this.resolveBranchId()
                )
                .set(
                    'page',
                    query.page
                )
                .set(
                    'size',
                    query.size
                );

        if (query.sort) {
            params =
                params.set(
                    'sort',
                    query.sort
                );
        }

        return this.http
            .get<any>(
                this.baseUrl,
                { params }
            )
            .pipe(
                map(response => ({
                    content:
                        response.content ?? [],

                    pageNumber:
                        response.number ?? 0,

                    pageSize:
                        response.size ?? 25,

                    totalElements:
                        response.totalElements ?? 0,

                    totalPages:
                        response.totalPages ?? 0,

                    last:
                        response.last ?? true
                }))
            );
    }

    get(
        id: string
    ): Observable<Account> {

        return this.http.get<Account>(
            `${this.baseUrl}/${id}`
        );
    }

    create(
        payload: Omit<
            CreateAccountRequest,
            'branchId'
        >
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

    deactivate(
        id: string
    ): Observable<void> {

        return this.http.post<void>(
            `${this.adminUrl}/${id}/deactivate`,
            {}
        );
    }
}