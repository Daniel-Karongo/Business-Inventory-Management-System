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

export interface InventoryWorkspaceResponse {

    productId: string;
    productName: string;
    productSku: string;

    productVariantId: string;
    productVariantSku: string;
    productClassification: string;

    branchId: string;

    quantityOnHand: number;
    quantityReserved: number;
    quantityAvailable: number;

    averageCost: number;

    inventoryValue: number;

    batchCount: number;

    oldestBatchDate?: string | null;

    createdAt?: string | null;

    updatedAt?: string | null;
}