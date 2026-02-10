import { BulkImportConfig } from
  '../../../../shared/bulk-import/models/bulk-import-config.model';
import { BulkRequest } from
  '../../../../shared/models/bulk-import.model';

export interface CustomerBulkRow {
  name: string;
  phones?: string;
  emails?: string;
  type?: 'INDIVIDUAL' | 'COMPANY';
  gender?: string;
  address?: string;
  notes?: string;
}

export const CUSTOMER_BULK_IMPORT_CONFIG: BulkImportConfig<
  CustomerBulkRow,
  any,
  any
> = {

  title: 'Bulk Import Customers',
  confirmLabel: 'Import Now',

  csvFileName: 'customers-bulk-template.csv',
  excelFileName: 'customers-bulk-template.xlsx',

  headers: [
    'name',
    'phones',
    'emails',
    'type',
    'gender',
    'address',
    'notes'
  ],

  fields: [
    { name: 'name', required: true },
    { name: 'phones' },
    { name: 'emails' },
    { name: 'type', defaultValue: 'INDIVIDUAL' },
    { name: 'gender' },
    { name: 'address' },
    { name: 'notes' }
  ],

  previewColumns: [
    { key: 'name', label: 'Name' },
    { key: 'phoneNumbers', label: 'Phones' },
    { key: 'email', label: 'Emails' },
    { key: 'type', label: 'Type' }
  ],

  excelSheetName: 'Customers',
  excelColumns: [
    { header: 'name', key: 'name', width: 22 },
    { header: 'phones', key: 'phones', width: 22, format: 'text' },
    { header: 'emails', key: 'emails', width: 32 },
    { header: 'type', key: 'type', width: 14 },
    { header: 'gender', key: 'gender', width: 14 },
    { header: 'address', key: 'address', width: 26 },
    { header: 'notes', key: 'notes', width: 32 }
  ],

  exampleRow: {
    name: 'John Doe',
    phones: '0712345678',
    emails: 'john@doe.com',
    type: 'INDIVIDUAL',
    gender: 'MALE',
    address: 'Nairobi',
    notes: 'VIP customer'
  },

  emptyRow: {
    type: 'INDIVIDUAL'
  },

  emptyRowCount: 200,

  defaultOptions: {
    dryRun: true,
    skipDuplicates: true
  },

  mapRowToItem(row: CustomerBulkRow) {
    return {
      name: row.name,
      phone: row.phones,
      email: row.emails,
      type: row.type,
      gender: row.gender || null,
      address: row.address,
      notes: row.notes
    };
  },

  mapPreviewRow(row: any) {
    return row;
  },

  submit(_: BulkRequest<any>) {
    return null as any;
  }
};