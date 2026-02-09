import { BulkRequest, BulkResult } from '../../models/bulk-import.model';

export interface BulkImportColumn {
  key: string;
  label: string;
}

export interface BulkImportField {
  name: string;
  required?: boolean;
  defaultValue?: any;
}

/* 🔴 NEW */
export interface BulkImportExcelColumn {
  header: string;
  key: string;
  width?: number;
  format?: 'text' | 'date' | 'number';
}

export interface BulkImportConfig<TRow, TItem, TResult> {

  /* UI */
  title: string;
  confirmLabel?: string;

  /* Files */
  csvFileName: string;
  excelFileName: string;

  /* Parsing */
  headers: string[];

  /* Form */
  fields: BulkImportField[];

  /* Preview */
  previewColumns?: BulkImportColumn[];

  /* 🔴 Excel Template Config */
  excelSheetName?: string;
  excelColumns?: BulkImportExcelColumn[];
  exampleRow?: Partial<TRow>;
  emptyRow?: Partial<TRow>;
  emptyRowCount?: number;

  defaultOptions?: {
    dryRun?: boolean;
    skipDuplicates?: boolean;
  };

  mapRowToItem(row: TRow): TItem;

  submit(request: BulkRequest<TItem>): any; // Observable<BulkResult<TResult>>
}