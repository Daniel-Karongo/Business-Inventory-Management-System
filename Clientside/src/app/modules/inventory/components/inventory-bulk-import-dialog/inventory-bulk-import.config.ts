import { BulkImportConfig } from
  '../../../../shared/bulk-import/models/bulk-import-config.model';
import { BulkRequest } from
  '../../../../shared/models/bulk-import.model';

export interface InventoryBulkRow {
  productName: string;
  variantClassification?: string;
  branchCode: string;
  supplierName: string;
  unitsSupplied: number;
  unitCost: number;
  sellingPrice?: number | null;
  reference?: string;
  note?: string;
}

export const INVENTORY_BULK_IMPORT_CONFIG: BulkImportConfig<
  InventoryBulkRow,
  any,
  any
> = {

  title: 'Bulk Receive Inventory',
  confirmLabel: 'Confirm Receive',

  csvFileName: 'inventory-bulk-receive-template.csv',
  excelFileName: 'inventory-bulk-receive-template.xlsx',

  headers: [
    'productName',
    'variantClassification',
    'branchCode',
    'supplierName',
    'unitsSupplied',
    'unitCost',
    'sellingPrice',
    'reference',
    'note'
  ],

  fields: [
    { name: 'productName', required: true },
    { name: 'variantClassification', defaultValue: 'STANDARD' },
    { name: 'branchCode', required: true },
    { name: 'supplierName', required: true },
    { name: 'unitsSupplied', required: true, defaultValue: 1 },
    { name: 'unitCost', required: true },
    { name: 'sellingPrice' },
    { name: 'reference' },
    { name: 'note' }
  ],

  previewColumns: [
    { key: 'productName', label: 'Product' },
    { key: 'variantClassification', label: 'Variant' },
    { key: 'branchName', label: 'Branch' },      // ✅ FIX
    { key: 'unitsReceived', label: 'Units' },    // ✅ FIX
    { key: 'unitCost', label: 'Unit Cost' },
    { key: 'totalCost', label: 'Total Cost' }
  ],

  excelSheetName: 'Inventory Receive',
  excelColumns: [
    { header: 'productName', key: 'productName', width: 30 },
    { header: 'variantClassification', key: 'variantClassification', width: 22 },
    { header: 'branchCode', key: 'branchCode', width: 18 },
    { header: 'supplierName', key: 'supplierName', width: 26 },
    { header: 'unitsSupplied', key: 'unitsSupplied', width: 16 },
    { header: 'unitCost', key: 'unitCost', width: 16 },
    { header: 'sellingPrice', key: 'sellingPrice', width: 16 },
    { header: 'reference', key: 'reference', width: 18 },
    { header: 'note', key: 'note', width: 28 }
  ],

  exampleRow: {
    productName: 'Milk 1L',
    variantClassification: 'STANDARD',
    branchCode: 'MAIN',
    supplierName: 'Local Supplier',
    unitsSupplied: 10,
    unitCost: 2500,
    sellingPrice: 3200,
    reference: 'GRN-001',
    note: 'Initial stock'
  },

  emptyRow: {
    variantClassification: 'STANDARD',
    unitsSupplied: 1
  },

  emptyRowCount: 200,

  defaultOptions: {
    dryRun: true
  },

  mapRowToItem(row: InventoryBulkRow) {
    return {
      productName: row.productName,
      classification: row.variantClassification,
      branchCode: row.branchCode,
      sellingPrice: row.sellingPrice,
      reference: row.reference,
      note: row.note,
      suppliers: [{
        supplierName: row.supplierName,
        unitsSupplied: row.unitsSupplied,
        unitCost: row.unitCost
      }]
    };
  },

  mapPreviewRow(row: any) {
    return row; // already UI-ready
  },

  submit(_: BulkRequest<any>) {
    return null as any;
  }
};