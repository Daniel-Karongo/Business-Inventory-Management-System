import { Injectable } from '@angular/core';
import { BulkAssignedFile } from './bulk-file.model';
import { BulkFileUtilsService } from './bulk-file-utils.service';

@Injectable({ providedIn: 'root' })
export class BulkFileLifecycleService {

  constructor(private utils: BulkFileUtilsService) {}

  revokeFile(file?: BulkAssignedFile | null) {
    if (!file?.previewUrl) return;
    this.utils.revokePreview(file.previewUrl);
  }

  revokeFiles(files: BulkAssignedFile[] = []) {
    files.forEach(f => this.revokeFile(f));
  }

  revokeRows(rows: any[]) {
    rows.forEach(row => {
      this.revokeFiles(row.files ?? []);
    });
  }
}