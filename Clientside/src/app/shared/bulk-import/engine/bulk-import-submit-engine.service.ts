import { Injectable } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { MatSnackBar } from '@angular/material/snack-bar';
import { Observable } from 'rxjs';

import { BulkRequest, BulkResult } from '../../models/bulk-import.model';
import { BulkImportResultDialogComponent } from
  '../../components/bulk-import-result-dialog/bulk-import-result-dialog.component';

/* ============================================================
   Inventory backend preview adapter helpers
   ============================================================ */

type PreviewContainer = {
  rows: any[];
};

function hasRowsContainer(x: unknown): x is PreviewContainer {
  return !!x && typeof x === 'object' && Array.isArray((x as any).rows);
}

/* ============================================================
   Submit Engine
   ============================================================ */

@Injectable({ providedIn: 'root' })
export class BulkImportSubmitEngineService {

  constructor(
    private dialog: MatDialog,
    private snackbar: MatSnackBar
  ) { }

  execute<T>({
    submitFn,
    payload,
    rows,
    dryRun,
    title,
    confirmLabel = 'Import Now',
    columns,
    onFinalSuccess,
    onConfirmRetry,
    onErrorsApplied
  }: {
    submitFn: (req: BulkRequest<any>) => Observable<BulkResult<T>>;
    payload: BulkRequest<any>;
    rows: any[];
    dryRun: boolean;
    title: string;
    confirmLabel?: string;
    columns?: { key: string; label: string }[];
    onFinalSuccess: (res: BulkResult<T>) => void;
    onConfirmRetry: () => void;
    onErrorsApplied?: (res: BulkResult<T>) => void;
  }) {

    submitFn(payload).subscribe({
      next: res => {

        /* ======================================================
           INVENTORY BACKEND NORMALIZATION
           ------------------------------------------------------
           Inventory returns preview rows inside:
             result.data[n].rows

           BulkImportResultDialogComponent renders from:
             result.data

           So we normalize ONCE here.
           ====================================================== */

        const dataAsUnknown = res.data as unknown[];

        if (Array.isArray(dataAsUnknown)) {
          const previewContainer = dataAsUnknown.find(hasRowsContainer);

          if (previewContainer) {
            res = {
              ...res,
              data: previewContainer.rows
            };
          }
        }

        /* ======================================================
           APPLY FIELD-LEVEL ERRORS
           ====================================================== */

        rows.forEach(r =>
          r.patchValue({ _error: '' }, { emitEvent: false })
        );

        res.errors?.forEach(e => {
          const row = rows[e.row - 1];
          if (row) {
            row.patchValue({ _error: e.message });
          }
        });

        onErrorsApplied?.(res);

        /* ======================================================
           FINAL (NON-DRY-RUN) SUCCESS
           ====================================================== */

        if (!dryRun && res.failed === 0) {
          this.snackbar.open(
            `${res.success} records imported successfully`,
            'Close',
            { duration: 3000 }
          );
          onFinalSuccess(res);
          return;
        }

        /* ======================================================
           PREVIEW DIALOG
           ====================================================== */

        const ref = this.dialog.open(BulkImportResultDialogComponent, {
          width: '1100px',
          maxWidth: '95vw',
          data: {
            title,
            dryRun,
            result: res,
            confirmLabel,
            columns
          }
        });

        ref.afterClosed().subscribe(confirm => {
          // restore error navigation after dialog close
          onErrorsApplied?.(res);

          if (confirm) {
            onConfirmRetry();
          }
        });
      },

      error: () => {
        this.snackbar.open(
          'Bulk import failed',
          'Close',
          { duration: 3000 }
        );
      }
    });
  }

  executeMultipart<T>({
    submitFn,
    formData,
    title,
    dryRun,
    confirmLabel = 'Import Now',
    columns,
    onFinalSuccess,
    onConfirmRetry
  }: {
    submitFn: (fd: FormData) => Observable<BulkResult<T>>;
    formData: FormData;
    title: string;
    dryRun: boolean;
    confirmLabel?: string;
    columns?: any[];
    onFinalSuccess: (res: BulkResult<T>) => void;
    onConfirmRetry?: () => void;
  }) {

    submitFn(formData).subscribe({
      next: res => {

        /* ================================
           FINAL SUCCESS (NON-DRY RUN)
           ================================ */

        if (!dryRun && res.failed === 0) {
          this.snackbar.open(
            `${res.success} records imported successfully`,
            'Close',
            { duration: 3000 }
          );
          onFinalSuccess(res);
          return;
        }

        /* ================================
           PREVIEW OR FAILED
           ================================ */

        const ref = this.dialog.open(BulkImportResultDialogComponent, {
          width: '1100px',
          maxWidth: '95vw',
          data: {
            title,
            dryRun,
            result: res,
            confirmLabel,
            columns
          }
        });

        ref.afterClosed().subscribe(confirm => {
          if (confirm && onConfirmRetry) {
            onConfirmRetry();
          }
        });
      },

      error: () => {
        this.snackbar.open(
          'Bulk import failed',
          'Close',
          { duration: 3000 }
        );
      }
    });
  }
}