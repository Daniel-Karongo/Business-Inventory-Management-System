export interface SupplierUnit {
    supplierId: string;
    unitsSupplied: number;
    unitCost: number;
    vatInclusive?: boolean;
    vatRate?: number;
}

export interface ReceiveStockRequest {
    productId: string;
    productVariantId?: string;
    classification?: string;
    newVariantSku?: string;
    branchId: string;
    sellingPrice?: number;
    suppliers: SupplierUnit[];
    reference?: string;
    note?: string;
}

export interface TransferStockRequest {
    productVariantId: string;
    fromBranchId: string;
    toBranchId: string;
    quantity: number;
    destinationUnitCost?: number;
    reference?: string;
    note?: string;
}

export interface AdjustStockRequest {
    productVariantId: string;
    branchId: string;
    quantityDelta: number;
    unitCost?: number;
    reason?: string;
    reference?: string;
}