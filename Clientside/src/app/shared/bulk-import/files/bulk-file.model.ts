export type BulkFileType = 'image' | 'pdf' | 'other';

export interface BulkAssignedFile {

  id: string;

  file: File;

  fileType: BulkFileType;

  previewUrl?: string;

  /** Multi-row ownership */
  assignedRowIndexes: number[];

  /** Per-row variant assignment */
  rowVariantMap: Record<number, string[] | undefined>;

  /** Business flag */
  assignToEntity: boolean;

  description?: string;
}