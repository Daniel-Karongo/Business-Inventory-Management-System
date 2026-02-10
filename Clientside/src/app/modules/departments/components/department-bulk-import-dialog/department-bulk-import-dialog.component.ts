import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReactiveFormsModule } from '@angular/forms';
import { MatDialogRef } from '@angular/material/dialog';

import { DepartmentService } from '../../services/department.service';

import { BulkImportFormComponent } from
  '../../../../shared/bulk-import/base/bulk-import-form.component';
import { BulkImportSubmitEngineService } from
  '../../../../shared/bulk-import/engine/bulk-import-submit-engine.service';
import { BulkImportShellComponent } from
  '../../../../shared/bulk-import/shell/bulk-import-shell.component';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { DEPARTMENT_BULK_IMPORT_CONFIG } from './department-bulk-import.config';

@Component({
  standalone: true,
  selector: 'app-department-bulk-import-dialog',
  templateUrl: './department-bulk-import-dialog.component.html',
  styleUrls: ['./department-bulk-import-dialog.component.scss'],
  imports: [
    CommonModule,
    ReactiveFormsModule,
    BulkImportShellComponent,

    MatFormFieldModule,
    MatInputModule,
    MatCheckboxModule,
    MatButtonModule,
    MatIconModule
  ]
})
export class DepartmentBulkImportDialogComponent
  extends BulkImportFormComponent<any, any, any>
  implements OnInit {

  config = DEPARTMENT_BULK_IMPORT_CONFIG;

  constructor(
    private departmentService: DepartmentService,
    private submitEngine: BulkImportSubmitEngineService,
    private dialogRef: MatDialogRef<DepartmentBulkImportDialogComponent>
  ) {
    super();
  }

  ngOnInit() {
    this.initForm({ updateExisting: false });
  }

  submit() {
    if (this.form.invalid) return;

    const payload = {
      items: this.rows.controls.map(r =>
        this.config.mapRowToItem(r.value)
      ),
      options: {
        dryRun: this.form.value.dryRun,
        updateExisting: this.form.value.updateExisting,
        skipDuplicates: true
      }
    };

    this.submitEngine.execute({
      submitFn: req => this.departmentService.bulkImport(req),
      payload,
      rows: this.rows.controls,
      dryRun: this.form.value.dryRun,
      title: this.config.title,
      confirmLabel: this.config.confirmLabel,
      columns: this.config.previewColumns,
      onErrorsApplied: res => {
        this.cacheErrors(res);

        if (this.config.mapPreviewRow && Array.isArray(res.data)) {
          res.data = res.data.map(r => this.config.mapPreviewRow!(r));
        }
      },
      onFinalSuccess: () => this.dialogRef.close(true),
      onConfirmRetry: () => {
        this.form.patchValue({ dryRun: false });
        this.submit();
      }
    });
  }

  async importExcel(file: File | undefined) {
    if (!file) return;
    const mode = await this.confirmMerge();
    if (!mode) return;
    super.importExcelFile(file, undefined, mode);
  }

  async importCsv(file: File | undefined) {
    if (!file) return;
    const mode = await this.confirmMerge();
    if (!mode) return;
    super.importCsvFile(file, mode);
  }

  close() {
    this.dialogRef.close();
  }
}