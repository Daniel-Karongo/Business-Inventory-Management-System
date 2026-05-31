import { Injectable } from '@angular/core';

import {
    HttpClient,
    HttpParams
} from '@angular/common/http';

import {
    map,
    Observable
} from 'rxjs';

import { environment }
    from '../../../../../../../environments/environment';

import { ApiResponse }
    from '../../../../../../core/models/api-response.model';

import {
    ReleaseStockRequest,
    ReserveStockRequest
} from '../../models/reservation.model';

import {
    InventoryValuationDashboard
} from '../../models/valuation.model';

import {
    StockOnboardingRequest,
    StockOnboardingResponse
} from '../../models/stock-onboarding.model';
import { PageWrapper } from '../../../../../../core/models/page-wrapper.model';

import {
    StockTransactionDTO
} from '../../models/stock-transaction.model';
import { InventoryResponse, InventoryWorkspaceResponse } from '../../models/inventory-response.model';
import { BatchConsumptionDTO, InventoryBatchDTO } from '../../models/inventory-batch.model';
import { AllocationPreviewDTO } from '../../models/allocation.model';
import { PreviewAllocationRequest } from '../../models/reservation.model';
import { AdjustStockRequest, ReceiveStockRequest, TransferStockRequest } from '../../models/stock-operation.model';
import { BulkRequest } from '../../../../../../shared/models/bulk-import.model';
import { InventoryBulkRow } from '../components/inventory-bulk-import-dialog/inventory-bulk-import.config';
import { InventoryBulkResult } from '../../models/inventory-bulk.model';

@Injectable({
    providedIn: 'root'
})
export class InventoryService {

    private api =
        environment.apiUrl;

    private endpoints =
        environment.endpoints.stock;

    constructor(
        private http: HttpClient
    ) { }

    /* =====================================================
       INVENTORY
    ===================================================== */

    getAll(
        page = 0,
        size = 50
    ): Observable<PageWrapper<InventoryResponse>> {

        const params =
            new HttpParams()
                .set('page', page)
                .set('size', size);

        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.inventory.list,
                { params }
            )
            .pipe(
                map(res => res.data as PageWrapper<InventoryResponse>)
            );
    }

    getByBranch(
        branchId: string,
        page = 0,
        size = 50
    ): Observable<PageWrapper<InventoryResponse>> {

        const params =
            new HttpParams()
                .set('page', page)
                .set('size', size);

        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.inventory.byBranch(branchId),
                { params }
            )
            .pipe(
                map(res => res.data as PageWrapper<InventoryResponse>)
            );
    }

    getVariantAcrossBranches(
        variantId: string
    ): Observable<InventoryResponse[]> {

        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.inventory.variantAll(variantId)
            )
            .pipe(
                map(res => res.data)
            );
    }

    getVariantStock(
        variantId: string,
        branchId: string
    ): Observable<InventoryWorkspaceResponse | null> {

        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.inventory.variantBranch(
                    variantId,
                    branchId
                )
            )
            .pipe(
                map(res => res.data ?? null)
            );
    }

    getProductAcrossBranches(
        productId: string
    ) {

        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.inventory
                    .productAcrossBranches(productId)
            )
            .pipe(
                map(res => res.data)
            );
    }

    getProductInBranch(
        productId: string,
        branchId: string
    ) {

        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.inventory
                    .productInBranch(
                        productId,
                        branchId
                    )
            )
            .pipe(
                map(res => res.data)
            );
    }

    /* =====================================================
       BATCHES
    ===================================================== */

    getBatches(
        variantId: string,
        branchId: string
    ): Observable<InventoryBatchDTO[]> {

        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.batches
                    .variantBranch(
                        variantId,
                        branchId
                    )
            )
            .pipe(
                map(res => res.data ?? [])
            );
    }

    suggestBatches(
        variantId: string,
        branchId: string,
        quantity: number
    ) {

        const params =
            new HttpParams()
                .set('variantId', variantId)
                .set('branchId', branchId)
                .set('quantity', quantity);

        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.batches.suggest,
                { params }
            )
            .pipe(
                map(res => res.data)
            );
    }

    getBatchConsumptions(
        batchId: string,
        branchId: string
    ): Observable<BatchConsumptionDTO[]> {

        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.batches
                    .consumptions(batchId),
                {
                    params: new HttpParams()
                        .set('branchId', branchId)
                }
            )
            .pipe(
                map(res => res.data ?? [])
            );
    }

    /* =====================================================
       RESERVATION PREVIEW
    ===================================================== */

    previewAllocation(
        payload:
            PreviewAllocationRequest
    ): Observable<AllocationPreviewDTO> {

        return this.http
            .post<ApiResponse>(
                this.api +
                this.endpoints.reservations.preview,
                payload
            )
            .pipe(
                map(res => res.data)
            );
    }

    reserveStock(
        payload: ReserveStockRequest
    ) {
        return this.http.post(
            this.api +
            this.endpoints.reservations.reserve,
            payload
        );
    }

    releaseReservation(
        payload: ReleaseStockRequest
    ) {
        return this.http.post(
            this.api +
            this.endpoints.reservations.release,
            payload
        );
    }

    /* =====================================================
       TRANSACTIONS
    ===================================================== */

    getTransactionsByVariant(
        branchId: string,
        variantId: string
    ): Observable<StockTransactionDTO[]> {

        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.transactions
                    .byVariant(
                        branchId,
                        variantId
                    )
            )
            .pipe(
                map(res => res.data ?? [])
            );
    }

    /* =====================================================
       OPERATIONS
    ===================================================== */

    bulkReceive(
        payload: BulkRequest<InventoryBulkRow>
    ): Observable<InventoryBulkResult> {

        return this.http
            .post<ApiResponse<InventoryBulkResult>>(
                this.api +
                this.endpoints.bulk.receive,
                payload
            )
            .pipe(
                map(res => {

                    if (!res.data) {
                        throw new Error(
                            'Bulk receive response contained no data'
                        );
                    }

                    return res.data;
                })
            );
    }

    receiveStock(
        payload: ReceiveStockRequest
    ) {

        return this.http.post(
            this.api +
            this.endpoints.operations.receive,
            payload
        );
    }

    transferStock(
        payload: TransferStockRequest
    ) {

        return this.http.post(
            this.api +
            this.endpoints.operations.transfer,
            payload
        );
    }

    adjustVariantStock(
        payload: AdjustStockRequest
    ) {

        return this.http.post(
            this.api +
            this.endpoints.operations.adjust,
            payload
        );
    }

    consumeStock(payload: {
        productVariantId: string;
        branchId: string;
        quantity: number;
        reference: string;
    }) {
        return this.http.post(
            this.api +
            this.endpoints.operations.consume,
            payload
        );
    }

    onboardStock(
        payload: StockOnboardingRequest
    ) {
        return this.http
            .post<ApiResponse<StockOnboardingResponse>>(
                this.api +
                this.endpoints.onboarding.create,
                payload
            )
            .pipe(
                map(res => res.data)
            );
    }

    /* =====================================================
       REPORTS
    ===================================================== */

    getLowStock(
        threshold = 10,
        page = 0,
        size = 50
    ): Observable<PageWrapper<InventoryResponse>> {

        const params =
            new HttpParams()
                .set('threshold', threshold)
                .set('page', page)
                .set('size', size);

        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.reports.lowStock,
                { params }
            )
            .pipe(
                map(res => res.data as PageWrapper<InventoryResponse>)
            );
    }

    getOutOfStock(
        page = 0,
        size = 50
    ): Observable<PageWrapper<InventoryResponse>> {

        const params =
            new HttpParams()
                .set('page', page)
                .set('size', size);

        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.reports.outOfStock,
                { params }
            )
            .pipe(
                map(res => res.data as PageWrapper<InventoryResponse>)
            );
    }

    getAuditTrail(
        productId: string
    ) {

        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.reports.audit(
                    productId
                )
            )
            .pipe(
                map(res => res.data)
            );
    }

    /* =====================================================
       VALUATION
    ===================================================== */

    getValuationDashboard() {
        return this.http
            .get<ApiResponse<InventoryValuationDashboard>>(
                this.api +
                this.endpoints.valuation.dashboard
            )
            .pipe(
                map(res => res.data as InventoryValuationDashboard)
            );
    }

    getBranchValuation(
        branchId: string
    ) {

        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.valuation.branch(
                    branchId
                )
            )
            .pipe(
                map(res => res.data)
            );
    }

    getProductValuation(
        productId: string
    ) {

        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.valuation.product(
                    productId
                )
            )
            .pipe(
                map(res => res.data)
            );
    }
    getCategoryValuation() {
        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.valuation.categories
            )
            .pipe(
                map(res => res.data)
            );
    }

    getHistoricalValuation(
        date?: string,
        method?: string
    ) {
        let params = new HttpParams();

        if (date) {
            params = params.set(
                'date',
                date
            );
        }

        if (method) {
            params = params.set(
                'method',
                method
            );
        }

        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.valuation.history,
                { params }
            )
            .pipe(
                map(res => res.data)
            );
    }

    getHistoricalVariantValuation(
        variantId: string,
        branchId: string,
        date?: string
    ) {
        let params = new HttpParams()
            .set('branchId', branchId);

        if (date) {
            params = params.set(
                'date',
                date
            );
        }

        return this.http
            .get<ApiResponse>(
                this.api +
                this.endpoints.valuation.historyVariant(
                    variantId
                ),
                { params }
            )
            .pipe(
                map(res => res.data)
            );
    }
}