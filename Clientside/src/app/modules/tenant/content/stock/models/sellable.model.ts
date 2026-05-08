import { PageWrapper } from '../../../../../core/models/page-wrapper.model';
import { AllocationDetail, AllocationPreviewDTO } from './allocation.model';
import { PackagingDTO } from './packaging.model';
import { PricingAdjustment, PricingPreviewDTO } from './pricing.model';

export interface WarningDTO {
  type: string;
  message: string;
}

export interface BatchPreviewDTO {
  batchId: string;
  available: number;
  unitCost: number;
  receivedAt: string;
}

export interface SellableVariantDTO {
  productId: string;
  productName: string;
  productSku: string;

  variantId: string;
  variantSku: string;
  classification: string;

  quantityOnHand: number;
  quantityReserved: number;
  quantityAvailable: number;

  packagings: PackagingDTO[];

  pricingByPackaging: Record<string, PricingPreviewDTO>;

  batches?: BatchPreviewDTO[];

  allocation?: AllocationPreviewDTO;

  warnings: WarningDTO[];
}

export interface SellableResolveRequest {
  branchId: string;
  productVariantId: string;
  packagingId?: string;

  quantity?: number;

  customerId?: string;
  customerGroupId?: string;

  batchIds?: string[];
}

export interface SellableResolveResponse {
  productVariantId: string;
  packagingId: string;

  requestedQuantity: number;
  baseUnits: number;

  unitPrice: number;
  totalPrice: number;

  availableStock: number;

  totalCost: number;

  batchAllocations?: AllocationDetail[];

  adjustments: PricingAdjustment[];
}

export interface SellableProductRequest {
  branchId: string;

  search?: string;

  customerId?: string;
  customerGroupId?: string;

  quantity?: number;

  includePricing?: boolean;
  includeBatches?: boolean;
  includeAllocation?: boolean;

  page?: number;
  size?: number;
}

export interface SellableProductResponse {
  variants: PageWrapper<SellableVariantDTO>;
}