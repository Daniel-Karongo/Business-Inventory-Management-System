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
  deleted?: boolean;
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
  id?: string;
  classification?: string;
  minimumPercentageProfit?: number;
  minimumProfit?: number;
  sku?: string;
  deleted?: boolean;
  autoCreateBasePackaging?: boolean;
}

export interface VariantImage {
  fileName: string;
  url: string;
  thumbnailUrl: string;
  deleted: boolean;
}

export interface VariantAudit {
  id: string;
  action: string;
  fieldChanged?: string;
  oldValue?: string;
  newValue?: string;
  reason?: string;
  timestamp: string;
  productId: string;
  productName: string;
  variantId: string;
  classification: string;
  performedBy: string;
}

export interface VariantImageAudit {
  id: string;
  productVariantId: string;
  productName: string;
  classification: string;
  fileName: string;
  filePath: string;
  action: string;
  reason?: string;
  timestamp: string;
  performedBy: string;
}