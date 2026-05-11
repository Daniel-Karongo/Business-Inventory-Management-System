export interface BulkOptions {
  dryRun?: boolean;
  updateExisting?: boolean;
  skipDuplicates?: boolean;
}

export interface BulkError {
  row: number;
  message: string;
}

export interface BulkResult<T> {
  total: number;
  success: number;
  failed: number;
  data: T[];
  errors: BulkError[];
  preview?: T[]
}

export interface BulkRequest<T> {
  items: T[];
  options?: BulkOptions;
}

export interface BulkImportResultDialogData {
  title: string;
  dryRun: boolean;
  result: BulkResult<any>;

  columns: {
    key: string;
    label: string;
  }[];

  confirmLabel?: string;
  closeLabel?: string;
}