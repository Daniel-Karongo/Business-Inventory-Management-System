export interface ProductVariant {
  id: string;
  productId: string;
  productName: string;
  classification: string;
  minimumPercentageProfit?: number;
  minimumProfit?: number;
  sku?: string;
  barcode?: string;
  barcodeImagePath?: string;
  imageUrls?: string[];
}