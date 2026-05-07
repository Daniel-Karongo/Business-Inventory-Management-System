export type OnboardingMode =
    | 'NEW_PRODUCT'
    | 'EXISTING_PRODUCT';

export interface OnboardingProductDraft {
    name: string;
    description?: string;
    categoryId?: number | null;
    minimumPercentageProfit?: number | null;
}

export interface OnboardingVariantDraft {
    id?: string | null;
    classification: string;
    sku?: string | null;
    barcode?: string | null;
}

export interface OnboardingPackagingDraft {
    tempId: string;
    name: string;
    unitQuantity: number;
    barcode?: string | null;
    isBaseUnit: boolean;
}

export interface OnboardingPricingDraft {
    packagingTempId: string;
    sellingPrice: number;
}

export interface OnboardingSupplierEntry {
    supplierId: string;
    packagingTempId: string;
    quantity: number;
    unitCost: number;
}

export interface StockOnboardingState {
    currentStep: number;

    mode: OnboardingMode;

    branchId: string | null;

    selectedProductId: string | null;

    selectedVariantId: string | null;

    productDraft: OnboardingProductDraft;

    variantDraft: OnboardingVariantDraft;

    packagings: OnboardingPackagingDraft[];

    pricing: OnboardingPricingDraft[];

    suppliers: OnboardingSupplierEntry[];

    notes?: string | null;

    reference?: string | null;
}