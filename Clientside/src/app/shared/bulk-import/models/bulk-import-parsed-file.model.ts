export interface BulkImportParsedFile<T> {
  rows: T[];
  total: number;
}

export type BulkImportMergeMode = 'replace' | 'append';