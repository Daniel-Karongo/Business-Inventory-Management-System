import { ProductVariant } from './product-variant.model';

export interface SupplierMinimalDTO {
  id: string;
  name: string;
}

export interface Product {
  id: string;
  name: string;
  description?: string;
  sku?: string;
  barcode?: string;
  barcodeImagePath?: string;
  branchId?: string;
  minimumPercentageProfit?: number;
  minimumProfit?: number;
  variants: ProductVariant[];
  imageUrls: string[];
  thumbnail?: string | null;
  categoryId?: number;
  categoryName?: string;
  suppliers: SupplierMinimalDTO[];
  deleted: boolean;
  deletedAt?: string;
  createdAt?: string;
  updatedAt?: string;
}

export interface ProductCreateDTO {
  name: string;
  description?: string;
  barcode?: string;
  categoryId: number;
  minimumPercentageProfit?: number;
  minimumProfit?: number;
  supplierIds?: string[];
  variants?: string[];
}

export interface ProductUpdateDTO {
  name?: string;
  description?: string;
  sku?: string;
  barcode?: string;
  minimumPercentageProfit?: number;
  minimumProfit?: number;
  categoryId?: number;
  supplierIds?: string[];
  branchId?: string;
}

export interface ProductImage {
  id: string;
  fileName: string;
  filePath: string;
  thumbnailFileName: string;
  deleted: boolean;
  primaryImage: boolean;
  deletedIndependently: boolean;
}

export interface ProductAudit {
  action: string;
  fieldChanged?: string;
  oldValue?: string;
  newValue?: string;
  reason?: string;
  performedBy?: string;
  timestamp: string;
}

export interface ProductImageAudit {
  id: string;
  fileName: string;
  filePath: string;
  action: string;
  reason?: string;
  timestamp: string;
  productId: string;
  productName: string;
  performedBy: string;
}