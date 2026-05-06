import { PageWrapper } from '../../../../../core/models/page-wrapper.model';
import { PackagingDTO } from './packaging.model';
import { PricingPreviewDTO } from './pricing.model';
import { AllocationPreviewDTO } from './allocation.model';

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

  pricingByPackaging:
  Record<string, PricingPreviewDTO>;

  batches?: BatchPreviewDTO[];

  allocation?: AllocationPreviewDTO;

  warnings: WarningDTO[];
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
  variants:
  PageWrapper<SellableVariantDTO>;
}