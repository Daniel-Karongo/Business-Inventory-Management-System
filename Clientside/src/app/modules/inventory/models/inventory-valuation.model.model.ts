export interface InventoryValuationItem {
  productId: string;
  productName: string;

  productVariantId: string;
  productVariantName: string;

  branchId: string;
  branchName: string;

  quantityOnHand: number;
  unitCost: number;           // average / weighted
  totalValue: number;         // quantity * unitCost
}

export interface InventoryValuationSummary {
  totalQuantity: number;
  totalValue: number;
}