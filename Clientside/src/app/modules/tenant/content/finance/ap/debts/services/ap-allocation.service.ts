import { HttpClient } from '@angular/common/http';
import { Injectable, inject } from '@angular/core';
import { Observable } from 'rxjs';
import {
    AllocateSupplierPaymentRequest,
    AllocationPreviewResponse,
    AllocationResponse,
    AutoAllocatePaymentRequest
} from '../models/allocation.model';
import { environment } from '../../../../../../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class ApAllocationService {
    private http = inject(HttpClient);

    private readonly baseUrl =
        `${environment.apiUrl}/finance/ap/allocations`;

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
}