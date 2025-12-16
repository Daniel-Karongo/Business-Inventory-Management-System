import { ProductVariant } from '../../variant/models/product-variant.model';

export interface Product {
  id: string;
  name: string;
  description?: string;
  sku?: string;
  barcode?: string;
  barcodeImagePath?: string;
  minimumPercentageProfit?: number;

  categoryId?: number;
  categoryName?: string;

  variants: ProductVariant[];
  imageUrls: string[];

  suppliers: {
    id: string;
    name: string;
  }[];

  deleted: boolean;
  deletedAt?: string;

  createdAt?: string;
  updatedAt?: string;
}