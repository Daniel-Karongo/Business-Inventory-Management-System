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

export interface ProductVariantCreateDTO {
  productId: string;
  classification: string;
  minimumPercentageProfit?: number;
  minimumProfit?: number;
  sku?: string;
  autoCreateBasePackaging?: boolean;
}

export interface ProductVariantUpdateDTO {
  classification?: string;

  minimumPercentageProfit?: number;
  minimumProfit?: number;

  sku?: string;
}