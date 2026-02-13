import { Injectable } from '@angular/core';
import { v4 as uuid } from 'uuid';
import { BulkAssignedFile } from './bulk-file.model';
import { BulkFileUtilsService } from './bulk-file-utils.service';

@Injectable({ providedIn: 'root' })
export class BulkFileFactoryService {

  constructor(private utils: BulkFileUtilsService) { }

  createAssignedFile(
    file: File,
    variants: string[] = [],
    defaults: { assignToEntity?: boolean } = {}
  ): BulkAssignedFile {

    const fileType = this.utils.detectFileType(file);

    return {
      id: uuid(),
      file,
      fileType,
      previewUrl: this.utils.createPreview(file),
      assignedRowIndexes: [],
      rowVariantMap: {},
      assignToEntity: defaults.assignToEntity ?? false,
      description: undefined
    };
  }
}