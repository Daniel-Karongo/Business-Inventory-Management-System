export interface InventoryResponse {
  productId: string;
  productName: string;
  productSKU: string;

  productVariantId: string;
  productClassification: string;
  productVariantSKU: string;

  branchId: string;
  branchName: string;

  averageCost?: number;

  quantityOnHand: number;
  quantityReserved: number;
  quantityAvailable: number;

  projectedNextSaleCost?: number;

  sellingPrice?: number;

  marginAmount?: number;
  marginPercent?: number;

  projectedMarginAmount?: number;
  projectedMarginPercent?: number;

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
  branchName?: string;

  quantityOnHand: number;
  quantityReserved: number;
  quantityAvailable: number;

  averageCost: number;
  projectedNextSaleCost: number;

  inventoryValue: number;

  sellingPrice?: number;

  marginAmount?: number;
  marginPercent?: number;

  projectedMarginAmount?: number;
  projectedMarginPercent?: number;

  batchCount: number;

  totalRemainingBatchValue?: number;

  oldestBatchDate?: string | null;

  lastReceiptDate?: string | null;
  lastReceiptQuantity?: number | null;

  lastSaleDate?: string | null;
  lastSaleQuantity?: number | null;

  lastTransferInDate?: string | null;
  lastTransferInQuantity?: number | null;

  lastTransferOutDate?: string | null;
  lastTransferOutQuantity?: number | null;

  createdAt?: string | null;
  updatedAt?: string | null;
}