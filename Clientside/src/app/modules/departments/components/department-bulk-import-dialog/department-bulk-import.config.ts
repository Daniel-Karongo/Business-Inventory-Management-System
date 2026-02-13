import { BulkImportConfig } from
  '../../../../shared/bulk-import/models/bulk-import-config.model';
import { BulkRequest } from
  '../../../../shared/models/bulk-import.model';

export interface DepartmentBulkRow {
  name: string;
  description?: string;
  rollcallStartTime?: string;
  gracePeriodMinutes?: number;
  branchCodes?: string;
  headUsernames?: string;
  memberUsernames?: string;
}

export const DEPARTMENT_BULK_IMPORT_CONFIG: BulkImportConfig<
  DepartmentBulkRow,
  any,
  any
> = {

  title: 'Bulk Import Departments',
  confirmLabel: 'Import Now',

  csvFileName: 'departments-bulk-template.csv',
  excelFileName: 'departments-bulk-template.xlsx',

  supportsArchiveImport: false,
  
  headers: [
    'name',
    'description',
    'rollcallStartTime',
    'gracePeriodMinutes',
    'branchCodes',
    'headUsernames',
    'memberUsernames'
  ],

  fields: [
    { name: 'name', required: true },
    { name: 'description' },
    { name: 'rollcallStartTime', defaultValue: '09:00' },
    { name: 'gracePeriodMinutes', defaultValue: 15 },
    { name: 'branchCodes', required: true, defaultValue: 'MAIN' },
    { name: 'headUsernames' },
    { name: 'memberUsernames' }
  ],

  previewColumns: [
    { key: 'name', label: 'Name' },
    { key: 'branches', label: 'Branches' },
    { key: 'heads', label: 'Heads' },
    { key: 'members', label: 'Members' }
  ],

  excelSheetName: 'Departments',
  excelColumns: [
    { header: 'name', key: 'name', width: 26 },
    { header: 'description', key: 'description', width: 36 },
    { header: 'rollcallStartTime', key: 'rollcallStartTime', width: 20, format: 'text' },
    { header: 'gracePeriodMinutes', key: 'gracePeriodMinutes', width: 22 },
    { header: 'branchCodes', key: 'branchCodes', width: 22, format: 'text' },
    { header: 'headUsernames', key: 'headUsernames', width: 28, format: 'text' },
    { header: 'memberUsernames', key: 'memberUsernames', width: 32, format: 'text' }
  ],

  exampleRow: {
    name: 'Management',
    description: 'Cash handling and budgeting',
    rollcallStartTime: '09:00',
    gracePeriodMinutes: 15,
    branchCodes: 'MAIN',
    headUsernames: 'Samuel Kimani;Mary Wanjiku',
    memberUsernames: 'Catherine Wairimu'
  },

  emptyRowCount: 200,

  defaultOptions: {
    dryRun: true
  },

  mapRowToItem(row: DepartmentBulkRow) {
    return {
      name: row.name,
      description: row.description,
      rollcallStartTime: row.rollcallStartTime,
      gracePeriodMinutes:
        row.gracePeriodMinutes != null
          ? Number(row.gracePeriodMinutes)
          : null,
      branchCodes: row.branchCodes
        ? row.branchCodes.split(',').map(v => v.trim())
        : [],
      headUsernames: row.headUsernames
        ? row.headUsernames.split(',').map(v => v.trim())
        : [],
      memberUsernames: row.memberUsernames
        ? row.memberUsernames.split(',').map(v => v.trim())
        : []
    };
  },

  mapPreviewRow(row: any) {
    return {
      ...row,
      branches: Array.isArray(row.branches)
        ? row.branches.map((b: any) => b.branchCode || b.name).join(', ')
        : '',
      heads: Array.isArray(row.heads)
        ? row.heads.map((h: any) => h.username).join(', ')
        : '',
      members: Array.isArray(row.members)
        ? row.members.map((m: any) => m.username).join(', ')
        : ''
    };
  },

  submit(_: BulkRequest<any>) {
    return null as any;
  }
};