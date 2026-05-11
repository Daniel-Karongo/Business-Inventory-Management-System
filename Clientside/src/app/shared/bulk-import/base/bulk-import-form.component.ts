import { Directive, inject } from '@angular/core';
import { FormArray, FormBuilder, FormGroup } from '@angular/forms';

import { MatDialog } from '@angular/material/dialog';
import { BulkImportClearDialogComponent } from '../component/bulk-import-clear-dialog.component';
import { BulkImportMergeDialogComponent } from '../component/bulk-import-merge-dialog.component';
import { BulkImportConfigAdapter } from '../config/bulk-import-config-adapter';
import { BulkImportEngineService } from '../engine/bulk-import-engine.service';
import { BulkImportTemplateService } from '../engine/bulk-import-template.service';
import { BulkImportConfig } from '../models/bulk-import-config.model';
import { BulkImportMergeMode } from '../models/bulk-import-parsed-file.model';
import { BulkImportBaseComponent } from './bulk-import-base.component';

@Directive()
export abstract class BulkImportFormComponent<
  TRow,
  TItem,
  TResult
> extends BulkImportBaseComponent<TResult> {

  protected fb = inject(FormBuilder);
  protected engine = inject(BulkImportEngineService);
  protected templates = inject(BulkImportTemplateService);
  protected dialog = inject(MatDialog);

  form!: FormGroup;
  abstract config: BulkImportConfig<TRow, TItem, TResult>;

  public loading = false;
  public progress: number | null = null;

  /* ================= INIT ================= */

  protected initForm(extra?: Record<string, any>) {
    this.form = BulkImportConfigAdapter.createForm(this.fb, this.config);
    this.addRow();

    if (extra) {
      Object.entries(extra).forEach(([k, v]) =>
        this.form.addControl(k, this.fb.control(v))
      );
    }
  }

  /* ================= ROWS ================= */

  get rows(): FormArray<FormGroup> {
    return this.form.get('rows') as FormArray<FormGroup>;
  }

  get rowCount(): number {
    return this.rows.length;
  }

  get supportsArchiveImport(): boolean {
    return !!this.config.supportsArchiveImport;
  }

  addRow(data?: Partial<TRow>) {

    BulkImportConfigAdapter.addRow(
      this.fb,
      this.form,
      this.config,
      data
    );

    const lastIndex = this.rows.length - 1;

    const control = this.rows.at(lastIndex); // now FormGroup

    if (!control.contains('files')) {
      control.addControl('files', this.fb.control([]));
    }

    if (!control.contains('variants')) {
      control.addControl('variants', this.fb.control([]));
    }
  }

  removeRow(index: number) {
    this.rows.removeAt(index);
  }

  clearRows() {
    if (!this.rows.length) return;

    const ref = this.dialog.open(BulkImportClearDialogComponent, {
      width: '420px',
    });

    ref.afterClosed().subscribe(confirm => {
      if (confirm === true) {
        this.clearRowsInternal();
      }
    });
  }

  protected override get errorRows(): number[] {
    const rows = this.form?.get('rows') as any;

    if (!rows?.controls) return [];

    return rows.controls
      .map((c: any, i: number) => c.value?._error ? i + 1 : null)
      .filter((x: number | null): x is number => x !== null);
  }

  protected cacheErrors(_: any) {
    const errors = this.errorRows;

    if (errors.length) {
      setTimeout(() => {

        this.scroll.goToLine(
          errors[0]
        );

      }, 100);
    }
  }

  protected clearRowsInternal() {
    this.rows.clear();
  }

  /* ================= FILE ================= */

  protected getFile(e: Event): File | null {
    return (e.target as HTMLInputElement).files?.[0] ?? null;
  }

  /* ================= CSV ================= */

  downloadCsvTemplate() {
    this.templates.downloadCsv(this.config);
  }

  importCsvFile(
    file: File,
    mode: BulkImportMergeMode = 'replace'
  ) {
    this.loading = true;
    this.progress = 0;

    if (mode === 'replace') {
      this.clearRowsInternal();
    }

    this.engine.importCsv(
      file,
      this.config.headers,
      (row, index?: number) => {
        this.addRow(row);

      },
      count => {

        this.loading = false;

        this.progress = null;

        const self =
          this as any;

        const hasErrors =
          typeof self.errorRows !== 'undefined'
          && Array.isArray(self.errorRows)
          && self.errorRows.length > 0;

        if (!hasErrors) {

          this.notifySuccess(
            `✅ ${count} lines loaded from CSV`
          );

        }

        queueMicrotask(() => {

          const self =
            this as any;

          if (typeof self.afterRowsImported === 'function') {

            self.afterRowsImported();

          }

        });

      },
      p => (this.progress = p)
    );
  }


  /* ================= EXCEL ================= */

  downloadExcelTemplate() {
    this.templates.downloadExcel(this.config);
  }

  importExcelFile(
    file: File,
    adapt?: (row: any) => Partial<any>,
    mode: BulkImportMergeMode = 'replace'
  ) {
    this.loading = true;
    this.progress = 0;

    if (mode === 'replace') {
      this.clearRowsInternal();
    }

    this.engine.importExcel(
      file,
      this.config.headers,
      adapt ?? null,
      (row, index?: number) => {
        this.addRow(row);

      },
      count => {

        this.loading = false;

        this.progress = null;

        const self =
          this as any;

        const hasErrors =
          typeof self.errorRows !== 'undefined'
          && Array.isArray(self.errorRows)
          && self.errorRows.length > 0;

        if (!hasErrors) {

          this.notifySuccess(
            `✅ ${count} lines loaded from Excel`
          );

        }

        queueMicrotask(() => {

          const self =
            this as any;

          if (typeof self.afterRowsImported === 'function') {

            self.afterRowsImported();

          }

        });

      },
      p => {
        this.progress = p;
      }
    );
  }

  protected async confirmMerge(): Promise<BulkImportMergeMode | null> {
    if (this.rowCount === 0) {
      return 'replace';
    }

    const ref = this.dialog.open(BulkImportMergeDialogComponent, {
      width: '440px',
      autoFocus: 'dialog'
    });

    const result = await ref.afterClosed().toPromise();

    // 🔴 Backdrop click / ESC / Cancel
    if (result !== 'append' && result !== 'replace') {
      this.snackbar.open('Import cancelled', 'Close', {
        duration: 2500
      });
      return null;
    }

    return result;
  }

  exportCsvCurrentRows() {
    const rows = this.rows.value;
    if (!rows.length) return;

    this.templates.exportCsvFromRows(rows, this.config);
  }

  exportExcelCurrentRows() {
    const rows = this.rows.value;
    if (!rows.length) return;

    this.templates.exportExcelFromRows(rows, this.config);
  }
}
