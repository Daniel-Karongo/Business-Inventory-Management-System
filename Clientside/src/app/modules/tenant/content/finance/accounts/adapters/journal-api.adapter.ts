import {
    Injectable,
    inject
} from '@angular/core';

import {
    HttpClient,
    HttpParams
} from '@angular/common/http';

import { map, Observable }
    from 'rxjs';

import { environment }
    from '../../../../../../../environments/environment';

import {
    Journal,
    JournalReversalRequest
} from '../models/journal.models';

import { TableQuery }
    from '../models/state/table-query.model';

import { PageWrapper }
    from '../../../../../../core/models/page-wrapper.model';

import { BranchContextService }
    from '../../../../../../core/services/branch-context.service';

@Injectable({
    providedIn: 'root'
})
export class JournalApiAdapter {

    private readonly http =
        inject(HttpClient);

    private readonly branchContext =
        inject(BranchContextService);

    private readonly baseUrl =
        `${environment.apiUrl}/accounting/journals`;

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
    ): Observable<PageWrapper<Journal>> {

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

        return this.http.get<any>
            (
                this.baseUrl,
                { params }
            ).pipe(
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
    ): Observable<Journal> {

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