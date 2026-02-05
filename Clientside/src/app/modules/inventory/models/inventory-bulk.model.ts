import { BulkResult } from '../../../shared/models/bulk-import.model';

export interface InventoryBulkPreviewRow {
  productId: string;
  productName: string;

  productVariantId?: string;
  variantClassification: string;
  variantSku: string;

  branchId: string;
  branchName: string;

  unitsReceived: number;
  unitCost: number;
  sellingPrice?: number;
  totalCost: number;
}

export interface InventoryBulkPreviewResult {
  rows: InventoryBulkPreviewRow[];
  totalUnits: number;
  totalCost: number;
}

export type InventoryBulkResult =
  BulkResult<InventoryBulkPreviewResult>;