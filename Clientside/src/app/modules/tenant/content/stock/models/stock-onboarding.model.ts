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
    newSupplierName?: string;
    unitsSupplied: number;
    unitCost: number;
    packagingName: string;
}

export interface StockOnboardingRequest {
    productName?: string;
    productId?: string;

    categoryId?: number;
    newCategoryName?: string;
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