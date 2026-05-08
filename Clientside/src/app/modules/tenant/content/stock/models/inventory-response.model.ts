export interface InventoryResponse {
  productId: string;
  productName: string;
  productSKU: string;

  productVariantId: string;
  productClassification: string;
  productVariantSKU: string;

  branchId: string;

  averageCost?: number;

  quantityOnHand: number;
  quantityReserved: number;
  quantityAvailable: number;

  batchCount?: number;
  oldestBatchDate?: string | null;
  totalRemainingBatchValue?: number;

  lastUpdatedAt: string | null;
}