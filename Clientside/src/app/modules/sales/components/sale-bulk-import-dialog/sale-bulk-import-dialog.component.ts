import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReactiveFormsModule } from '@angular/forms';
import { MatDialogRef } from '@angular/material/dialog';

import { SalesService } from '../../services/sales.service';
import { BulkImportFormComponent } from
  '../../../../shared/bulk-import/base/bulk-import-form.component';
import { BulkImportSubmitEngineService } from
  '../../../../shared/bulk-import/engine/bulk-import-submit-engine.service';
import { BulkImportShellComponent } from
  '../../../../shared/bulk-import/shell/bulk-import-shell.component';
import { SALE_BULK_IMPORT_CONFIG } from './sale-bulk-import.config';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatButtonModule } from '@angular/material/button';
import { MatNativeDateModule } from '@angular/material/core';
import { MatIconModule } from '@angular/material/icon';

@Component({
  standalone: true,
  selector: 'app-sale-bulk-import-dialog',
  templateUrl: './sale-bulk-import-dialog.component.html',
  styleUrls: ['./sale-bulk-import-dialog.component.scss'],
  imports: [
    CommonModule,
    ReactiveFormsModule,
    BulkImportShellComponent,

    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatCheckboxModule,
    MatDatepickerModule,
    MatNativeDateModule,
    MatButtonModule,
    MatIconModule
  ]
})
export class SaleBulkImportDialogComponent
  extends BulkImportFormComponent<any, any, any>
  implements OnInit {

  config = SALE_BULK_IMPORT_CONFIG;

  constructor(
    private salesService: SalesService,
    private submitEngine: BulkImportSubmitEngineService,
    private dialogRef: MatDialogRef<SaleBulkImportDialogComponent>
  ) {
    super();
  }

  ngOnInit() {
    this.initForm({ mode: 'OPERATIONAL' });
  }

  submit() {
    if (this.form.invalid) return;

    const payload = {
      items: this.rows.controls.map(r =>
        this.config.mapRowToItem(r.value)
      ),
      options: { dryRun: this.form.value.dryRun }
    };

    this.submitEngine.execute({
      submitFn: req =>
        this.salesService.import(this.form.value.mode, req),
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
    if (!mode) return; // ⬅ cancelled

    super.importExcelFile(
      file,
      row => ({
        ...row,
        saleDate: this.engine.parseExcelDate(row.saleDate)
      }),
      mode
    );
  }

  async importCsv(file: File | undefined) {
    if (!file) return;

    const mode = await this.confirmMerge();
    if (!mode) return; // ⬅ cancelled

    super.importCsvFile(file, mode);
  }

  close() {
    this.dialogRef.close();
  }
}