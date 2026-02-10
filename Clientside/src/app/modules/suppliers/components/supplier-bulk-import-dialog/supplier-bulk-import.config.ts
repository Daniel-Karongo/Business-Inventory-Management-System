import { BulkImportConfig } from
  '../../../../shared/bulk-import/models/bulk-import-config.model';
import { BulkRequest } from
  '../../../../shared/models/bulk-import.model';

export interface SupplierBulkRow {
  name: string;
  emails: string;
  phones: string;
  address?: string;
  region?: string;
  categories?: string;
}

export const SUPPLIER_BULK_IMPORT_CONFIG: BulkImportConfig<
  SupplierBulkRow,
  any,
  any
> = {

  title: 'Bulk Import Suppliers',
  confirmLabel: 'Import Now',

  csvFileName: 'suppliers-bulk-template.csv',
  excelFileName: 'suppliers-bulk-template.xlsx',

  headers: [
    'name',
    'emails',
    'phones',
    'address',
    'region',
    'categories'
  ],

  fields: [
    { name: 'name', required: true },
    { name: 'emails' },
    { name: 'phones', required: true },
    { name: 'address' },
    { name: 'region' },
    { name: 'categories' }
  ],

  previewColumns: [
    { key: 'name', label: 'Name' },
    { key: 'email', label: 'Emails' },
    { key: 'phoneNumber', label: 'Phones' },
    { key: 'region', label: 'Region' }
  ],

  excelSheetName: 'Suppliers',
  excelColumns: [
    { header: 'name', key: 'name', width: 26 },
    { header: 'emails', key: 'emails', width: 32 },
    { header: 'phones', key: 'phones', width: 22, format: 'text' },
    { header: 'address', key: 'address', width: 26 },
    { header: 'region', key: 'region', width: 18 },
    { header: 'categories', key: 'categories', width: 32 }
  ],

  exampleRow: {
    name: 'Acme Ltd',
    emails: 'info@acme.com',
    phones: '0712345678',
    address: 'Nairobi',
    region: 'Nairobi',
    categories: 'Electronics,Accessories'
  },

  emptyRowCount: 200,

  defaultOptions: {
    dryRun: true,
    skipDuplicates: true
  },

  mapRowToItem(row: SupplierBulkRow) {
    return {
      name: row.name,
      email: [row.emails],
      phoneNumber: [row.phones],
      address: row.address,
      region: row.region,
      categoryNames: row.categories
        ? row.categories.split(',').map(c => c.trim()).filter(Boolean)
        : []
    };
  },

  mapPreviewRow(row: any) {
    return row;
  },

  submit(_: BulkRequest<any>) {
    return null as any;
  }
};