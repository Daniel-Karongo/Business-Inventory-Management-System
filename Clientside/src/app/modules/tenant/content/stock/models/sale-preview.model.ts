import { AllocationDetail } from './allocation.model';
import { PricingAdjustment } from './pricing.model';

export interface BatchSelectionDto {
    batchId: string;
    quantity: number;
}

export interface SaleLinePreviewRequest {
    productVariantId: string;
    packagingId: string;

    quantity: number;

    customerId?: string;
    customerGroupId?: string;

    branchId: string;

    batchSelections?: BatchSelectionDto[];
    requestedUnitPrice?: number;
    overrideReason?: string;
}

export interface SaleLinePreviewResponse {
    productVariantId: string;
    packagingId: string;

    requestedQuantity: number;
    baseUnits: number;

    unitPrice: number;
    totalPrice: number;

    totalCost: number;

    availableStock: number;

    batchAllocations: AllocationDetail[];

    adjustments: PricingAdjustment[];
}