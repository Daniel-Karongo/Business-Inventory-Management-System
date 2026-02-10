import { BulkImportConfig } from
  '../../../../shared/bulk-import/models/bulk-import-config.model';
import { BulkRequest } from
  '../../../../shared/models/bulk-import.model';

export interface BranchBulkRow {
  branchCode: string;
  name: string;
  location?: string;
  phone?: string;
  email?: string;
}

export const BRANCH_BULK_IMPORT_CONFIG: BulkImportConfig<
  BranchBulkRow,
  BranchBulkRow,
  any
> = {

  title: 'Bulk Import Branches',
  confirmLabel: 'Import Now',

  csvFileName: 'branches-bulk-template.csv',
  excelFileName: 'branches-bulk-template.xlsx',

  headers: [
    'branchCode',
    'name',
    'location',
    'phone',
    'email'
  ],

  fields: [
    { name: 'branchCode', required: true },
    { name: 'name', required: true },
    { name: 'location' },
    { name: 'phone' },
    { name: 'email' }
  ],

  previewColumns: [
    { key: 'branchCode', label: 'Code' },
    { key: 'name', label: 'Name' },
    { key: 'location', label: 'Location' },
    { key: 'phone', label: 'Phone' },
    { key: 'email', label: 'Email' }
  ],

  excelSheetName: 'Branches',
  excelColumns: [
    { header: 'branchCode', key: 'branchCode', width: 14 },
    { header: 'name', key: 'name', width: 22 },
    { header: 'location', key: 'location', width: 22 },
    { header: 'phone', key: 'phone', width: 16, format: 'text' },
    { header: 'email', key: 'email', width: 28 }
  ],

  exampleRow: {
    branchCode: 'MAIN',
    name: 'Main Branch',
    location: 'Nairobi',
    phone: '0712345678',
    email: 'main@example.com'
  },

  emptyRowCount: 200,

  defaultOptions: {
    dryRun: true,
    skipDuplicates: true
  },

  mapRowToItem(row: BranchBulkRow) {
    return {
      branchCode: row.branchCode,
      name: row.name,
      location: row.location,
      phone: row.phone,
      email: row.email
    };
  },

  mapPreviewRow(row: any) {
    return row;
  },

  submit(_: BulkRequest<any>) {
    return null as any;
  }
};