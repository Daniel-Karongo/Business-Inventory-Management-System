import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReactiveFormsModule } from '@angular/forms';
import { MatDialogRef } from '@angular/material/dialog';

import { BranchService } from '../../services/branch.service';

import { BulkImportFormComponent } from
  '../../../../shared/bulk-import/base/bulk-import-form.component';
import { BulkImportSubmitEngineService } from
  '../../../../shared/bulk-import/engine/bulk-import-submit-engine.service';
import { BulkImportShellComponent } from
  '../../../../shared/bulk-import/shell/bulk-import-shell.component';

import { BRANCH_BULK_IMPORT_CONFIG } from './branch-bulk-import.config';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';

@Component({
  standalone: true,
  selector: 'app-branch-bulk-import-dialog',
  templateUrl: './branch-bulk-import-dialog.component.html',
  styleUrls: ['./branch-bulk-import-dialog.component.scss'],
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
export class BranchBulkImportDialogComponent
  extends BulkImportFormComponent<any, any, any>
  implements OnInit {

  config = BRANCH_BULK_IMPORT_CONFIG;

  constructor(
    private branchService: BranchService,
    private submitEngine: BulkImportSubmitEngineService,
    private dialogRef: MatDialogRef<BranchBulkImportDialogComponent>
  ) {
    super();
  }

  ngOnInit() {
    this.initForm();

    // auto-uppercase branchCode
    this.rows.valueChanges.subscribe(rows => {
      rows.forEach((r: any, i: number) => {
        if (r.branchCode) {
          const upper = r.branchCode.toUpperCase();
          if (r.branchCode !== upper) {
            this.rows.at(i)
              .get('branchCode')
              ?.setValue(upper, { emitEvent: false });
          }
        }
      });
    });
  }

  submit() {
    if (this.form.invalid) return;

    const payload = {
      items: this.rows.controls.map(r =>
        this.config.mapRowToItem(r.value)
      ),
      options: {
        dryRun: this.form.value.dryRun,
        skipDuplicates: true
      }
    };

    this.submitEngine.execute({
      submitFn: req => this.branchService.bulkImport(req),
      payload,
      rows: this.rows.controls,
      dryRun: this.form.value.dryRun,
      title: this.config.title,
      confirmLabel: this.config.confirmLabel,
      columns: this.config.previewColumns,
      onErrorsApplied: res => this.cacheErrors(res),
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