import {
  CommonModule
} from '@angular/common';

import {
  Component,
  OnInit,
  inject
} from '@angular/core';

import {
  ReactiveFormsModule
} from '@angular/forms';

import {
  MatButtonModule
} from '@angular/material/button';

import {
  MatCheckboxModule
} from '@angular/material/checkbox';

import {
  MatNativeDateModule
} from '@angular/material/core';

import {
  MatDatepickerModule
} from '@angular/material/datepicker';

import {
  MatDialogRef
} from '@angular/material/dialog';

import {
  MatFormFieldModule
} from '@angular/material/form-field';

import {
  MatIconModule
} from '@angular/material/icon';

import {
  MatInputModule
} from '@angular/material/input';

import {
  MatSelectModule
} from '@angular/material/select';

import {
  BulkImportFormComponent
} from '../../../../../../shared/bulk-import/base/bulk-import-form.component';

import {
  BulkImportSubmitEngineService
} from '../../../../../../shared/bulk-import/engine/bulk-import-submit-engine.service';

import {
  BulkImportShellComponent
} from '../../../../../../shared/bulk-import/shell/bulk-import-shell.component';

import {
  SALE_BULK_IMPORT_CONFIG
} from './sale-bulk-import.config';

import {
  SalesService
} from '../../services/sales.service';

@Component({
  standalone: true,
  selector: 'app-sale-bulk-import-dialog',
  templateUrl:
    './sale-bulk-import-dialog.component.html',
  styleUrls: [
    './sale-bulk-import-dialog.component.scss'
  ],
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
  extends BulkImportFormComponent<
    any,
    any,
    any
  >
  implements OnInit {

  private readonly salesService =
    inject(SalesService);

  private readonly submitEngine =
    inject(
      BulkImportSubmitEngineService
    );

  private readonly dialogRef =
    inject(
      MatDialogRef<
        SaleBulkImportDialogComponent
      >
    );

  readonly config =
    SALE_BULK_IMPORT_CONFIG;

  ngOnInit(): void {

    this.initForm({
      mode: 'OPERATIONAL',
      dryRun: true
    });
  }

  submit(): void {

    if (
      this.form.invalid ||
      this.submitting
    ) {
      return;
    }

    const payload = {
      items:
        this.rows.controls.map(
          row =>
            this.config
              .mapRowToItem(
                row.getRawValue()
              )
        ),
      options: {
        dryRun:
          this.form.value.dryRun
      }
    };

    this.submitEngine.execute({

      submitFn: request =>
        this.salesService.import(
          this.form.value.mode,
          request
        ),

      payload,

      rows:
        this.rows.controls,

      dryRun:
        this.form.value.dryRun,

      title:
        this.config.title,

      confirmLabel:
        this.config.confirmLabel,

      columns:
        this.config.previewColumns,

      onErrorsApplied:
        result =>
          this.cacheErrors(result),

      onFinalSuccess: () => {

        this.dialogRef.close(true);
      },

      onConfirmRetry: () => {

        this.form.patchValue({
          dryRun: false
        });

        this.submit();
      }
    });
  }

  async importExcel(
    file:
      File |
      undefined
  ): Promise<void> {

    if (!file) {
      return;
    }

    const mode =
      await this.confirmMerge();

    if (!mode) {
      return;
    }

    super.importExcelFile(
      file,
      row => ({
        ...row,
        saleDate:
          this.engine.parseExcelDate(
            row.saleDate
          )
      }),
      mode
    );
  }

  async importCsv(
    file:
      File |
      undefined
  ): Promise<void> {

    if (!file) {
      return;
    }

    const mode =
      await this.confirmMerge();

    if (!mode) {
      return;
    }

    super.importCsvFile(
      file,
      mode
    );
  }

  close(): void {

    this.dialogRef.close();
  }
}