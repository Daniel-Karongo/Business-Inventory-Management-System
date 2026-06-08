import {
    Injectable,
    inject
} from '@angular/core';

import {
    HttpClient,
    HttpParams
} from '@angular/common/http';

import {
    Observable
} from 'rxjs';

import {
    OperationalExpense,
    CreateOperationalExpenseRequest,
    SettleOperationalExpenseRequest,
    BulkSettleOperationalExpenseRequest
} from '../models/operational-expense.model';

import {
    OperationalExpenseWorkspace
} from '../models/operational-expense-workspace.model';

import {
    BranchContextService
} from '../../../../../../core/services/branch-context.service';

import {
    environment
} from '../../../../../../../environments/environment';

@Injectable({
    providedIn: 'root'
})
export class OperationalExpenseService {

    private readonly http =
        inject(HttpClient);

    private readonly branchContext =
        inject(BranchContextService);

    private readonly baseUrl =
        `${environment.apiUrl}/operational-expenses`;

    private branchId(): string {

        const id =
            this.branchContext.currentBranch;

        if (!id) {
            throw new Error(
                'Branch not selected'
            );
        }

        return id;
    }

    list(
        page = 0,
        size = 20,
        search?: string,
        status?: string
    ): Observable<any> {

        let params =
            new HttpParams()
                .set(
                    'page',
                    page
                )
                .set(
                    'size',
                    size
                );

        if (search) {
            params =
                params.set(
                    'search',
                    search
                );
        }

        if (status) {
            params =
                params.set(
                    'status',
                    status
                );
        }

        return this.http.get(
            `${this.baseUrl}/branch/${this.branchId()}`,
            { params }
        );
    }

    workspace(
        expenseId: string
    ): Observable<any> {
        return this.http.get(
            `${this.baseUrl}/branch/${this.branchId()}/${expenseId}`
        );
    }

    getOpenExpenses(): Observable<any> {
        return this.http.get(
            `${this.baseUrl}/branch/${this.branchId()}/open`
        );
    }

    create(
        request: CreateOperationalExpenseRequest
    ) {

        return this.http.post(
            this.baseUrl,
            request
        );
    }

    settle(
        expenseId: string,
        request: SettleOperationalExpenseRequest
    ) {

        return this.http.post(
            `${this.baseUrl}/${this.branchId()}/${expenseId}/settle`,
            request
        );
    }

    bulkSettle(
        request: BulkSettleOperationalExpenseRequest
    ) {

        return this.http.post(
            `${this.baseUrl}/${this.branchId()}/bulk-settle`,
            request
        );
    }

    reverseExpense(
        expenseId: string,
        reason: string
    ) {

        const params =
            new HttpParams()
                .set(
                    'reason',
                    reason
                );

        return this.http.post(
            `${this.baseUrl}/${this.branchId()}/${expenseId}/reverse`,
            {},
            { params }
        );
    }

    reverseSettlement(
        settlementId: string,
        reason: string
    ) {

        const params =
            new HttpParams()
                .set(
                    'reason',
                    reason
                );

        return this.http.post(
            `${this.baseUrl}/${this.branchId()}/settlements/${settlementId}/reverse`,
            {},
            { params }
        );
    }
}