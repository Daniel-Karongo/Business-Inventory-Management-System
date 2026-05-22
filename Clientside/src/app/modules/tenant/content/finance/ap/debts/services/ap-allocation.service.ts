import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable, inject } from '@angular/core';
import { Observable } from 'rxjs';
import {
    AllocateSupplierPaymentRequest,
    AllocationPreviewResponse,
    AllocationResponse,
    AutoAllocatePaymentRequest
} from '../models/allocation.model';
import { environment } from '../../../../../../../../environments/environment';
import { BranchContextService } from '../../../../../../../core/services/branch-context.service';

@Injectable({ providedIn: 'root' })
export class ApAllocationService {
    private http = inject(HttpClient);

    private readonly baseUrl =
        `${environment.apiUrl}/finance/ap/allocations`;

    private branchContext =
        inject(BranchContextService);

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

    manualAllocate(
        request: AllocateSupplierPaymentRequest
    ): Observable<AllocationResponse> {
        return this.http.post<AllocationResponse>(
            `${this.baseUrl}/manual`,
            request
        );
    }

    previewAutoAllocation(
        request: AutoAllocatePaymentRequest
    ): Observable<AllocationPreviewResponse> {
        return this.http.post<AllocationPreviewResponse>(
            `${this.baseUrl}/preview`,
            request
        );
    }

    autoAllocate(
        request: AutoAllocatePaymentRequest
    ): Observable<AllocationResponse[]> {
        return this.http.post<AllocationResponse[]>(
            `${this.baseUrl}/auto`,
            request
        );
    }

    reverse(
        allocationId: string,
        reason: string,
        overrideBranchId?: string
    ): Observable<AllocationResponse> {

        const params =
            new HttpParams()
                .set(
                    'branchId',
                    this.resolveBranchId(
                        overrideBranchId
                    )
                );

        return this.http.post<AllocationResponse>(
            `${this.baseUrl}/${allocationId}/reverse`,
            { reason },
            { params }
        );
    }
}