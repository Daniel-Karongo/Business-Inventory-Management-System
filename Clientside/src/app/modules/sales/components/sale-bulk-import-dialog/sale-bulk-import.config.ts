import { BulkImportConfig } from '../../../../shared/bulk-import/models/bulk-import-config.model';
import { BulkRequest } from '../../../../shared/models/bulk-import.model';

export interface SaleBulkRow {
  receiptNo: string;
  sku?: string;
  productName?: string;
  variant: string;
  quantity: number;
  unitPrice: number;
  branchCode: string;
  saleDate: Date | string | null;
  payments?: string;
}

export const SALE_BULK_IMPORT_CONFIG: BulkImportConfig<
  SaleBulkRow,
  any,
  any
> = {

  title: 'Bulk Import Sales',
  confirmLabel: 'Import Now',

  csvFileName: 'sales-bulk-template.csv',
  excelFileName: 'sales-bulk-template.xlsx',

  supportsArchiveImport: false,
  
  headers: [
    'receiptNo',
    'sku',
    'productName',
    'variant',
    'quantity',
    'unitPrice',
    'branchCode',
    'saleDate',
    'payments'
  ],

  fields: [
    { name: 'receiptNo', required: true },
    { name: 'sku' },
    { name: 'productName' },
    { name: 'variant', defaultValue: 'STANDARD' },
    { name: 'quantity', required: true, defaultValue: 1 },
    { name: 'unitPrice', required: true },
    { name: 'branchCode', defaultValue: 'MAIN' },
    { name: 'saleDate' },
    { name: 'payments' }
  ],

  previewColumns: [
    { key: 'receiptNo', label: 'Receipt' },
    { key: 'productName', label: 'Product' },
    { key: 'sku', label: 'SKU' },
    { key: 'quantity', label: 'Qty' },
    { key: 'unitPrice', label: 'Price' },
    { key: 'branchCode', label: 'Branch' }
  ],

  excelSheetName: 'Sales',
  excelColumns: [
    { header: 'receiptNo', key: 'receiptNo', width: 16 },
    { header: 'sku', key: 'sku', width: 16 },
    { header: 'productName', key: 'productName', width: 28 },
    { header: 'variant', key: 'variant', width: 14 },
    { header: 'quantity', key: 'quantity', width: 10 },
    { header: 'unitPrice', key: 'unitPrice', width: 14 },
    { header: 'branchCode', key: 'branchCode', width: 14 },
    { header: 'saleDate', key: 'saleDate', width: 18 },
    { header: 'payments', key: 'payments', width: 32 }
  ],

  exampleRow: {
    receiptNo: 'R-001',
    sku: 'SKU-001',
    variant: 'STANDARD',
    quantity: 2,
    unitPrice: 4500,
    branchCode: 'MAIN',
    payments: 'CASH|9000'
  },

  emptyRow: {
    variant: 'STANDARD',
    quantity: 1,
    branchCode: 'MAIN'
  },

  emptyRowCount: 200,

  defaultOptions: {
    dryRun: true,
    skipDuplicates: true
  },

  mapRowToItem(row: SaleBulkRow) {
    return {
      receiptNo: row.receiptNo,
      sku: row.sku || null,
      productName: row.productName || null,
      variant: row.variant,
      quantity: row.quantity,
      unitPrice: row.unitPrice,
      branchCode: row.branchCode,
      saleDate: row.saleDate
        ? new Date(row.saleDate).toISOString()
        : null,
      payments: row.payments || null
    };
  },

  submit(_: BulkRequest<any>) {
    return null as any;
  }
};