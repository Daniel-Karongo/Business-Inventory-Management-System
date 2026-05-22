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

import { LedgerRow }
    from '../models/ledger.models';

import { PageWrapper }
    from '../../../../../../core/models/page-wrapper.model';

import { TableQuery }
    from '../models/state/table-query.model';

import { BranchContextService }
    from '../../../../../../core/services/branch-context.service';

@Injectable({
    providedIn: 'root'
})
export class LedgerApiAdapter {

    private readonly http =
        inject(HttpClient);

    private readonly branchContext =
        inject(BranchContextService);

    private readonly baseUrl =
        `${environment.apiUrl}/accounting/ledger`;

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
        accountId: string,
        query: TableQuery
    ): Observable<PageWrapper<LedgerRow>> {

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

        return this.http.get<any>(
            `${this.baseUrl}/${accountId}`,
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
        );;
    }

}