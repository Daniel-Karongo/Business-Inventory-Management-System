export interface InventoryBatchDTO {
  batchId: string;
  productVariantId: string;
  branchId: string;

  unitCost: number;

  quantityReceived: number;
  quantityRemaining: number;

  totalRemainingValue: number;

  receivedAt: string;
}

export interface BatchConsumptionDTO {
  batchId: string;
  saleId?: string;
  productVariantId: string;

  quantity: number;
  unitCost: number;
  totalCost: number;
}