import { AllocationDetail } from './allocation.model';
import { PricingAdjustment } from './pricing.model';

export interface BarcodeScanRequest {
    branchId: string;
    barcode: string;
}

export interface BarcodeScanResponse {
    productId: string;
    productName: string;

    variantId: string;
    classification: string;

    sku: string;
    barcode: string;

    branchId: string;

    packagingId: string;

    requestedQuantity: number;
    baseUnits: number;

    unitPrice: number;
    totalPrice: number;

    availableStock: number;

    totalCost: number;

    batchAllocations: AllocationDetail[];

    adjustments: PricingAdjustment[];
}