import { BulkImportConfig } from "../../../../../shared/bulk-import/models/bulk-import-config.model";
import { BulkRequest } from "../../../../../shared/models/bulk-import.model";

export interface ProductBulkRow {
  name: string;
  description?: string;
  categoryName: string;
  supplierNames?: string;
  variants?: string;
  minimumPercentageProfit?: number;
}

export const PRODUCT_BULK_IMPORT_CONFIG: BulkImportConfig<
  ProductBulkRow,
  any,
  any
> = {

  title: 'Bulk Import Products',
  confirmLabel: 'Import Products',

  csvFileName: 'products-bulk-template.csv',
  excelFileName: 'products-bulk-template.xlsx',

  supportsArchiveImport: true,
  supportsFileDescription: false,

  headers: [
    'name',
    'description',
    'categoryName',
    'supplierNames',
    'variants',
    'minimumPercentageProfit'
  ],

  fields: [
    { name: 'name', required: true },
    { name: 'description' },
    { name: 'categoryName', required: true },
    { name: 'supplierNames' },
    { name: 'variants', defaultValue: 'STANDARD' },
    { name: 'minimumPercentageProfit' }
  ],

  previewColumns: [
    { key: 'name', label: 'Name' },
    { key: 'categoryName', label: 'Category' },
    { key: 'variantNames', label: 'Variants' },
    { key: 'minimumPercentageProfit', label: 'Min % Profit' }
  ],

  excelSheetName: 'Products',
  excelColumns: [
    { header: 'name', key: 'name', width: 26 },
    { header: 'description', key: 'description', width: 36 },
    { header: 'categoryName', key: 'categoryName', width: 22 },
    { header: 'supplierNames', key: 'supplierNames', width: 30 },
    { header: 'variants', key: 'variants', width: 20 },
    { header: 'minimumPercentageProfit', key: 'minimumPercentageProfit', width: 18 }
  ],

  exampleRow: {
    name: 'Milk 1L',
    description: 'Fresh whole milk',
    categoryName: 'Dairy',
    supplierNames: 'Local Supplier',
    variants: 'STANDARD',
    minimumPercentageProfit: 15
  },

  emptyRow: {
    variants: 'STANDARD'
  },

  emptyRowCount: 200,

  defaultOptions: {
    dryRun: true,
    skipDuplicates: true
  },

  mapRowToItem(row: ProductBulkRow) {
    return {
      name: row.name,
      description: row.description,
      categoryName: row.categoryName,
      supplierNames: row.supplierNames
        ? row.supplierNames.split(',').map(s => s.trim()).filter(Boolean)
        : [],
      variants: row.variants
        ? row.variants.split(',').map(v => v.trim()).filter(Boolean)
        : ['STANDARD'],
      minimumPercentageProfit: row.minimumPercentageProfit
    };
  },

  mapPreviewRow(row: any) {
    return {
      ...row,
      variantNames: Array.isArray(row.variants)
        ? row.variants.map((v: any) => v.classification).filter(Boolean).join(', ')
        : ''
    };
  },

  submit(_: BulkRequest<any>) {
    return null as any;
  }
};