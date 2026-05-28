export interface PricingAdjustment {
    type: string;
    source?: string;
    amount: number;
    description?: string;
}

export interface PricingPreviewDTO {
    unitPrice: number;
    totalPrice: number;
    adjustments: PricingAdjustment[];
}

export interface PricingBreakdownDTO {
    basePrice?: number;
    finalPrice?: number;
    adjustments: PricingAdjustment[];
}

export interface ProductPrice {
    id?: string;

    productVariant?: {
        id: string;
    };

    packaging?: {
        id: string;
    };

    price: number;
    minQuantity: number;
    deleted?: boolean;
}

export interface CreatePriceRequest {
    branchId: string;
    variantId: string;
    packagingId: string;
    price: number;
    minQuantity: number;
}

export interface UpdatePriceRequest {
    branchId: string;
    price: number;
}