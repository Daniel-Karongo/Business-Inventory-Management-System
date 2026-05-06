export interface SaleLineBatchSelection {
    batchId: string;
    quantity: number;
}

export interface PreviewAllocationRequest {
    variantId: string;
    branchId: string;

    quantity: number;

    selectedBatchIds?: string[];
}

export interface ReserveStockRequest {
    productVariantId: string;

    packagingId: string;

    branchId: string;

    baseUnits: number;

    quantity: number;

    reference?: string;

    batchSelections?: SaleLineBatchSelection[];
}

export interface ReleaseStockRequest {
    branchId: string;
    reference: string;
}