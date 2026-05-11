export interface PackagingInput {
    name: string;
    units: number;
}
export interface PricingInput {
    packagingName: string;
    sellingPrice: number;
}
export interface SupplierInput {
    supplierId?: string;
    supplierName?: string;
    createSupplierIfMissing?: boolean;
    unitsSupplied: number;
    unitCost: number;
    packagingName: string;
}
export interface StockOnboardingRequest {
    productName?: string;
    productId?: string;
    categoryId?: number;
    newCategoryName?: string;
    createCategoryIfMissing?: boolean;
    createProductIfMissing?: boolean;
    supplierIds?: string[];
    minimumPercentageProfit?: number;
    classification?: string;
    variantId?: string;
    packagings: PackagingInput[];
    pricing: PricingInput[];
    suppliers: SupplierInput[];
    branchId: string;
    reference?: string;
    note?: string;
}
export interface StockOnboardingResponse {
    productId: string;
    variantId: string;
    branchId: string;
    totalUnitsReceived: number;
    totalCost: number;
    message: string;
}
/* =====================================================
   BULK PREVIEW
===================================================== */
export interface StockOnboardingBulkPreviewRow {
    productId?: string;
    productName?: string;
    variantId?: string;
    classification?: string;
    branchId: string;
    totalUnits: number;
    totalCost: number;
    existingProduct: boolean;
    existingVariant: boolean;
    categoryCreated: boolean;
    suppliersCreated: number;
    packagingCount: number;
    pricingCount: number;
}
export interface StockOnboardingBulkPreviewResult {
    rows: StockOnboardingBulkPreviewRow[];
    totalUnits: number;
    totalCost: number;
}