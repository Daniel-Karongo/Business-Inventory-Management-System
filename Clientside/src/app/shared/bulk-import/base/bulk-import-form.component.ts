import { Directive, inject } from '@angular/core';
import { FormArray, FormBuilder, FormGroup } from '@angular/forms';

import { BulkImportBaseComponent } from './bulk-import-base.component';
import { BulkImportConfig } from '../models/bulk-import-config.model';
import { BulkImportConfigAdapter } from '../config/bulk-import-config-adapter';
import { BulkImportEngineService } from '../engine/bulk-import-engine.service';
import { BulkImportTemplateService } from '../engine/bulk-import-template.service';
import { BulkImportMergeMode } from '../models/bulk-import-parsed-file.model';
import { MatDialog } from '@angular/material/dialog';
import { BulkImportMergeDialogComponent } from '../component/bulk-import-merge-dialog.component';
import { BulkImportClearDialogComponent } from '../component/bulk-import-clear-dialog.component';


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

  get rows(): FormArray {
    return this.form.get('rows') as FormArray;
  }

  get rowCount(): number {
    return this.rows.length;
  }

  addRow(data?: Partial<TRow>) {
    BulkImportConfigAdapter.addRow(
      this.fb,
      this.form,
      this.config,
      data
    );
  }

  removeRow(index: number) {
    this.rows.removeAt(index);
  }

  protected clearRowsInternal() {
    this.rows.clear();

    // 🔴 Reset import-related state
    this.errorRows = [];
    this.errorIndex = 0;
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
      row => this.addRow(row),
      count => {
        this.loading = false;
        this.progress = null;
        this.notifySuccess(`✅ ${count} lines loaded from CSV`);
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
      row => this.addRow(row),
      count => {
        this.loading = false;
        this.progress = null;
        this.notifySuccess(`✅ ${count} lines loaded from Excel`);
      },
      p => (this.progress = p)
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
}