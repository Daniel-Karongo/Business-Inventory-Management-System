export interface ProductVariant {
  id: string;
  productId: string;
  productName: string;

  classification: string;

  sku?: string;
  averageBuyingPrice?: number;
  minimumSellingPrice?: number;
}