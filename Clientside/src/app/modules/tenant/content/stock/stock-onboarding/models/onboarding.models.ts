export type OnboardingMode =
    | 'NEW_PRODUCT'
    | 'EXISTING_PRODUCT';

export const NEW_CATEGORY =
    '__NEW_CATEGORY__' as const;

export interface OnboardingProductDraft {
    name: string;
    description?: string;
    categoryId?:
    | number
    | typeof NEW_CATEGORY
    | null;
    newCategoryName?: string | null;
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
    supplierId?: string;
    supplierName?: string;
    packagingId?: string;
    packagingName?: string;
    packagingTempId?: string;
    unitsSupplied: number;
    unitCost: number;
    vatInclusive: boolean;
    vatRate: number;
}

export type SupplierPaymentMethod =
    | 'CASH'
    | 'BANK'
    | 'MPESA';

export interface OnboardingOperationalExpense {
    expenseAccountId: string;
    description: string;
    amount: number;
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

    accountingDate: string | null;

    operationalExpenses: OnboardingOperationalExpense[];

    autoPaySuppliers: boolean;

    supplierPaymentMethod: SupplierPaymentMethod | null;

    autoPayOperationalExpenses: boolean;

    fundingAccountId: string | null;

    notes?: string | null;

    reference?: string | null;
}