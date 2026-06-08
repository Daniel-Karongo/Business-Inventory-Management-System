import {
    HttpClient,
    HttpParams
} from '@angular/common/http';
import {
    Injectable,
    inject
} from '@angular/core';
import {
    Observable,
    throwError
} from 'rxjs';
import {
    catchError
} from 'rxjs/operators';
import { environment } from '../../../../../../../../environments/environment';
import { BranchContextService } from '../../../../../../../core/services/branch-context.service';
import { PageWrapper } from '../../../../../../../core/models/page-wrapper.model';
import { ProcessSupplierPaymentRequest, SupplierPaymentResponseDto } from '../models/supplier-payment.model';
import {
    CreateSupplierPaymentRequest,
    ReverseSupplierPaymentRequest
} from '../models/create-supplier-payment-request.model';
import { FundingAccount } from '../models/funding-account.model';
import { SupplierPaymentDetailsDto } from '../models/supplier-payment-details.model';
import { BulkSupplierPaymentRequest, BulkSupplierPaymentResult } from '../models/bulk-supplier-payment.model';

@Injectable({
    providedIn: 'root'
})
export class ApPaymentService {

    private readonly http =
        inject(HttpClient);
    private readonly branchContext =
        inject(BranchContextService);
    private readonly baseUrl =
        `${environment.apiUrl}/finance/supplier-payments`;

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

    create(
        request: CreateSupplierPaymentRequest
    ): Observable<SupplierPaymentResponseDto> {
        return this.http
            .post<SupplierPaymentResponseDto>(
                this.baseUrl,
                request
            )
            .pipe(
                catchError(
                    this.handleError
                )
            );
    }

    post(
        paymentId: string,
        overrideBranchId?: string
    ): Observable<SupplierPaymentResponseDto> {
        const params =
            new HttpParams()
                .set(
                    'branchId',
                    this.resolveBranchId(
                        overrideBranchId
                    )
                );
        return this.http
            .post<SupplierPaymentResponseDto>(
                `${this.baseUrl}/${paymentId}/post`,
                {},
                { params }
            )
            .pipe(
                catchError(
                    this.handleError
                )
            );
    }

    process(
        request: ProcessSupplierPaymentRequest
    ): Observable<SupplierPaymentResponseDto> {

        return this.http
            .post<SupplierPaymentResponseDto>(
                `${this.baseUrl}/process`,
                request
            )
            .pipe(
                catchError(
                    this.handleError
                )
            );
    }

    reverse(
        paymentId: string,
        request: ReverseSupplierPaymentRequest,
        overrideBranchId?: string
    ): Observable<SupplierPaymentResponseDto> {
        const params =
            new HttpParams()
                .set(
                    'branchId',
                    this.resolveBranchId(
                        overrideBranchId
                    )
                );
        return this.http
            .post<SupplierPaymentResponseDto>(
                `${this.baseUrl}/${paymentId}/reverse`,
                request,
                { params }
            )
            .pipe(
                catchError(
                    this.handleError
                )
            );
    }

    details(
        paymentId: string,
        overrideBranchId?: string
    ): Observable<SupplierPaymentDetailsDto> {
        const params =
            new HttpParams()
                .set(
                    'branchId',
                    this.resolveBranchId(
                        overrideBranchId
                    )
                );
        return this.http
            .get<SupplierPaymentDetailsDto>(
                `${this.baseUrl}/${paymentId}`,
                { params }
            )
            .pipe(
                catchError(
                    this.handleError
                )
            );
    }

    fundingAccounts(
        overrideBranchId?: string
    ): Observable<FundingAccount[]> {
        const params =
            new HttpParams()
                .set(
                    'branchId',
                    this.resolveBranchId(
                        overrideBranchId
                    )
                );
        return this.http
            .get<FundingAccount[]>(
                `${this.baseUrl}/funding-accounts`,
                { params }
            )
            .pipe(
                catchError(
                    this.handleError
                )
            );
    }

    list(
        page = 0,
        size = 20,
        supplierId?: string,
        status?: string,
        overrideBranchId?: string
    ): Observable<
        PageWrapper<SupplierPaymentResponseDto>
    > {
        let params =
            new HttpParams()
                .set(
                    'branchId',
                    this.resolveBranchId(
                        overrideBranchId
                    )
                )
                .set('page', page)
                .set('size', size);
        if (supplierId) {
            params =
                params.set(
                    'supplierId',
                    supplierId
                );
        }
        if (status) {
            params =
                params.set(
                    'status',
                    status
                );
        }
        return this.http
            .get<
                PageWrapper<SupplierPaymentResponseDto>
            >(
                this.baseUrl,
                { params }
            )
            .pipe(
                catchError(
                    this.handleError
                )
            );
    }

    private handleError = (
        error: any
    ) => {
        const message =
            error?.error?.message
            ||
            error?.error
            ||
            'Operation failed';
        return throwError(
            () => new Error(message)
        );
    };

    bulkProcess(
        request: BulkSupplierPaymentRequest
    ): Observable<BulkSupplierPaymentResult[]> {

        return this.http.post<
            BulkSupplierPaymentResult[]
        >(
            `${this.baseUrl}/bulk`,
            request
        ).pipe(
            catchError(
                this.handleError
            )
        );
    }
}