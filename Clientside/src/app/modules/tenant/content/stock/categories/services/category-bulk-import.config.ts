import { BulkImportConfig } from "../../../../../../shared/bulk-import/models/bulk-import-config.model";
import { BulkRequest } from "../../../../../../shared/models/bulk-import.model";


export interface CategoryBulkRow {
  name: string;
  parentName?: string;
}

export const CATEGORY_BULK_IMPORT_CONFIG: BulkImportConfig<
  CategoryBulkRow,
  any,
  any
> = {

  title: 'Bulk Import Categories',
  confirmLabel: 'Import Categories',

  csvFileName: 'categories-template.csv',
  excelFileName: 'categories-template.xlsx',

  headers: [
    'name',
    'parentName'
  ],

  fields: [
    { name: 'name', required: true },
    { name: 'parentName' }
  ],

  previewColumns: [
    { key: 'name', label: 'Name' },
    { key: 'parentName', label: 'Parent' }
  ],

  excelSheetName: 'Categories',
  excelColumns: [
    { header: 'name', key: 'name', width: 28 },
    { header: 'parentName', key: 'parentName', width: 28 }
  ],

  exampleRow: {
    name: 'Electronics',
    parentName: ''
  },

  emptyRow: {},

  emptyRowCount: 200,

  defaultOptions: {
    dryRun: true,
    skipDuplicates: true
  },

  mapRowToItem(row: CategoryBulkRow) {
    return {
      name: row.name,
      parentName: row.parentName || null
    };
  },

  submit(_: BulkRequest<any>) {
    return null as any;
  }
};