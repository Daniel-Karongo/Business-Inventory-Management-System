import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReactiveFormsModule } from '@angular/forms';
import { MatDialogRef } from '@angular/material/dialog';

import { InventoryService } from '../../services/inventory.service';

import { BulkImportFormComponent } from
  '../../../../shared/bulk-import/base/bulk-import-form.component';
import { BulkImportSubmitEngineService } from
  '../../../../shared/bulk-import/engine/bulk-import-submit-engine.service';
import { BulkImportShellComponent } from
  '../../../../shared/bulk-import/shell/bulk-import-shell.component';

import { INVENTORY_BULK_IMPORT_CONFIG } from './inventory-bulk-import.config';
import { MatIconModule } from '@angular/material/icon';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';

@Component({
  standalone: true,
  selector: 'app-inventory-bulk-import-dialog',
  templateUrl: './inventory-bulk-import-dialog.component.html',
  styleUrls: ['./inventory-bulk-import-dialog.component.scss'],
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
export class InventoryBulkImportDialogComponent
  extends BulkImportFormComponent<any, any, any>
  implements OnInit {

  config = INVENTORY_BULK_IMPORT_CONFIG;

  constructor(
    private inventoryService: InventoryService,
    private submitEngine: BulkImportSubmitEngineService,
    private dialogRef: MatDialogRef<InventoryBulkImportDialogComponent>
  ) {
    super();
  }

  ngOnInit() {
    this.initForm();
  }

  submit() {
    if (this.form.invalid) return;

    const payload = {
      items: this.rows.controls.map(r =>
        this.config.mapRowToItem(r.value)
      ),
      options: {
        dryRun: this.form.value.dryRun
      }
    };

    this.submitEngine.execute({
      submitFn: req => this.inventoryService.bulkImport(req),
      payload,
      rows: this.rows.controls,
      dryRun: this.form.value.dryRun,
      title: this.config.title,
      confirmLabel: this.config.confirmLabel,
      columns: this.config.previewColumns,
      onErrorsApplied: r => this.cacheErrors(r),
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