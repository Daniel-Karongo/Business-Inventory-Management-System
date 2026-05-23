import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable, inject } from '@angular/core';
import { Observable } from 'rxjs';

import { environment } from '../../../../../../../../environments/environment';

import { BranchContextService } from '../../../../../../../core/services/branch-context.service';

import { PageWrapper } from '../../../../../../../core/models/page-wrapper.model';
import { PageQuery } from '../../../../../../../core/models/page-query.model';

import { SupplierDebtSummary } from '../models/supplier-debt-summary.model';
import { SupplierWorkspaceDto } from '../models/supplier-workspace.model';

@Injectable({ providedIn: 'root' })
export class ApDebtService {

    private http = inject(HttpClient);

    private branchContext =
        inject(BranchContextService);

    private readonly baseUrl =
        `${environment.apiUrl}/finance/ap/debts`;

    private resolveBranchId(
        override?: string
    ): string {

        const branchId =
            override ?? this.branchContext.currentBranch;

        if (!branchId) {
            throw new Error('Branch not selected');
        }

        return branchId;
    }

    getDebtSummary(
        query: PageQuery = {},
        overrideBranchId?: string
    ): Observable<PageWrapper<SupplierDebtSummary>> {

        const branchId =
            this.resolveBranchId(overrideBranchId);

        let params = new HttpParams()
            .set('branchId', branchId)
            .set('page', query.page ?? 0)
            .set('size', query.size ?? 20);

        if (query.sortBy) {
            params =
                params.set('sortBy', query.sortBy);
        }

        if (query.direction) {
            params =
                params.set('direction', query.direction);
        }

        if (query.search?.trim()) {
            params =
                params.set('search', query.search.trim());
        }

        if (query.hasOverdue !== undefined) {
            params =
                params.set(
                    'hasOverdue',
                    query.hasOverdue
                );
        }

        if (query.hasUnapplied !== undefined) {
            params =
                params.set(
                    'hasUnapplied',
                    query.hasUnapplied
                );
        }

        return this.http.get<
            PageWrapper<SupplierDebtSummary>
        >(
            this.baseUrl,
            { params }
        );
    }

    getWorkspace(
        supplierId: string,
        overrideBranchId?: string
    ): Observable<SupplierWorkspaceDto> {

        const branchId =
            this.resolveBranchId(overrideBranchId);

        const params = new HttpParams()
            .set('branchId', branchId);

        return this.http.get<SupplierWorkspaceDto>(
            `${this.baseUrl}/${supplierId}`,
            { params }
        );
    }
}