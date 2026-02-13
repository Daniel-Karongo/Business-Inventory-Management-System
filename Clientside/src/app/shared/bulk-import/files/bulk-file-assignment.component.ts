import {
  Component,
  Input,
  Output,
  EventEmitter,
  OnChanges
} from '@angular/core';

import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatIconModule } from '@angular/material/icon';
import { MatDialog } from '@angular/material/dialog';

import { BulkAssignedFile } from './bulk-file.model';
import { BulkFileUtilsService } from './bulk-file-utils.service';
import { BulkFilePreviewDialogComponent } from './bulk-file-preview-dialog.component';

@Component({
  standalone: true,
  selector: 'app-bulk-file-assignment',
  imports: [
    CommonModule,
    FormsModule,              // ✅ required for ngModel
    MatCheckboxModule,
    MatIconModule
  ],
  templateUrl: './bulk-file-assignment.component.html',
  styleUrls: ['./bulk-file-assignment.component.scss']
})
export class BulkFileAssignmentComponent {

  @Input() files: BulkAssignedFile[] = [];
  @Input() rowIndex!: number;
  @Input() rowVariants: string[] = [];
  @Input() supportsDescription = false;
  @Input() multiSelectMode = false;

  @Output() multiAssign = new EventEmitter<BulkAssignedFile[]>();
  @Output() remove = new EventEmitter<string>();
  @Output() assign = new EventEmitter<BulkAssignedFile>();

  selectedIds = new Set<string>();

  constructor(
    private utils: BulkFileUtilsService,
    private dialog: MatDialog
  ) { }

  getPreview(file: BulkAssignedFile): string | null {

    if (!file.previewUrl) {
      const preview = this.utils.createPreview(file.file);

      if (!preview) return null;

      file.previewUrl = preview;
    }

    return file.previewUrl ?? null;
  }

  isImage(file: BulkAssignedFile): boolean {
    return this.utils.isImage(file.file);
  }

  toggleSelection(file: BulkAssignedFile, checked: boolean) {
    if (checked) {
      this.selectedIds.add(file.id);
    } else {
      this.selectedIds.delete(file.id);
    }
  }

  onVariantChange(
    event: Event,
    file: BulkAssignedFile,
    variant: string
  ) {
    const input = event.target as HTMLInputElement;
    if (!input) return;

    let variants = file.rowVariantMap[this.rowIndex];

    if (!variants) {
      variants = [];
      file.rowVariantMap[this.rowIndex] = variants;
    }

    if (input.checked) {
      if (!variants.includes(variant)) {
        variants.push(variant);
      }
    } else {
      file.rowVariantMap[this.rowIndex] =
        variants.filter(v => v !== variant);
    }
  }

  preview(file: BulkAssignedFile) {

    const url = this.getPreview(file);

    this.dialog.open(
      BulkFilePreviewDialogComponent,
      {
        data: {
          url,
          isImage: this.isImage(file)
        },
        maxWidth: '95vw'
      }
    );
  }

  removeFile(id: string) {
    this.remove.emit(id);
  }

  trackByFile(_: number, file: BulkAssignedFile) {
    return file.id;
  }

  emitMultiAssign() {
    const selected = this.files.filter(f =>
      this.selectedIds.has(f.id)
    );

    this.multiAssign.emit(selected);
    this.selectedIds.clear();
  }

  clearSelection() {
    this.selectedIds.clear();
  }
}