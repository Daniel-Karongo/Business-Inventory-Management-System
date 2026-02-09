import { BulkImportConfig } from
  '../../../../shared/bulk-import/models/bulk-import-config.model';
import { BulkRequest } from
  '../../../../shared/models/bulk-import.model';

export interface UserBulkRow {
  username: string;
  password?: string;
  role?: string;
  emails?: string;
  phones?: string;
  branchCode?: string;
  departmentName?: string;
  position?: string;
}

export const USER_BULK_IMPORT_CONFIG: BulkImportConfig<
  UserBulkRow,
  any,
  any
> = {

  title: 'Bulk Import Users',
  confirmLabel: 'Import Now',

  csvFileName: 'users-bulk-template.csv',
  excelFileName: 'users-bulk-template.xlsx',

  headers: [
    'username',
    'password',
    'role',
    'emails',
    'phones',
    'branchCode',
    'departmentName',
    'position'
  ],

  fields: [
    { name: 'username', required: true },
    { name: 'password', defaultValue: '1234' },
    { name: 'role', defaultValue: 'EMPLOYEE' },
    { name: 'emails' },
    { name: 'phones' },
    { name: 'branchCode' },
    { name: 'departmentName' },
    { name: 'position', defaultValue: 'member' }
  ],

  previewColumns: [
    { key: 'username', label: 'Username' },
    { key: 'role', label: 'Role' },
    { key: 'emails', label: 'Emails' },
    { key: 'phones', label: 'Phones' },
    { key: 'branchCode', label: 'Branch' },
    { key: 'departmentName', label: 'Department' }
  ],

  excelSheetName: 'Users',
  excelColumns: [
    { header: 'username', key: 'username', width: 18 },
    { header: 'password', key: 'password', width: 12 },
    { header: 'role', key: 'role', width: 14 },
    { header: 'emails', key: 'emails', width: 32 },
    { header: 'phones', key: 'phones', width: 22, format: 'text' },
    { header: 'branchCode', key: 'branchCode', width: 14 },
    { header: 'departmentName', key: 'departmentName', width: 18 },
    { header: 'position', key: 'position', width: 12 }
  ],

  exampleRow: {
    username: 'jdoe',
    password: '1234',
    role: 'EMPLOYEE',
    emails: 'jdoe@company.com',
    phones: '0712345678',
    branchCode: 'MAIN',
    departmentName: 'GENERAL',
    position: 'member'
  },

  emptyRow: {
    password: '1234',
    role: 'EMPLOYEE',
    position: 'member'
  },

  emptyRowCount: 200,

  defaultOptions: {
    dryRun: true,
    skipDuplicates: true
  },

  mapRowToItem(row: UserBulkRow) {
    return {
      username: row.username,
      password: row.password,
      role: row.role,
      emailAddresses: row.emails ? [row.emails] : [],
      phoneNumbers: row.phones ? [row.phones] : [],
      branchCode: row.branchCode || null,
      departmentName: row.departmentName || null,
      position: row.position
    };
  },

  mapPreviewRow(row: any) {
    return {
      ...row,
      emails: Array.isArray(row.emailAddresses)
        ? row.emailAddresses.join(', ')
        : '',
      phones: Array.isArray(row.phoneNumbers)
        ? row.phoneNumbers.join(', ')
        : ''
    };
  },

  submit(_: BulkRequest<any>) {
    return null as any;
  }
};