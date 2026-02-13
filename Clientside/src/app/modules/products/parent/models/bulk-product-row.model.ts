import { BulkAssignedFile } from
'../../../../shared/bulk-import/files/bulk-file.model';

export interface BulkProductRow {

  rowId: string;

  name: string;
  description?: string;

  categoryName: string;

  supplierNames: string[];

  minimumPercentageProfit?: number;

  variants: string[];

  files: BulkAssignedFile[];
}