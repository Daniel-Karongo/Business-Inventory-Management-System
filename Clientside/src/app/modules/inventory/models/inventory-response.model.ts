export interface InventoryResponse {
  productId: string;
  productName: string;
  productSKU: string;

  productVariantId: string;
  productClassification: string;
  productVariantSKU: string;

  branchId: string;
  branchName: string;

  quantityOnHand: number;
  quantityReserved: number;

  lastUpdatedAt: string | null;
}