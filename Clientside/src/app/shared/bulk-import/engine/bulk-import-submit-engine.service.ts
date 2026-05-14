import { Injectable } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { MatSnackBar } from '@angular/material/snack-bar';
import { Observable } from 'rxjs';

import { BulkRequest, BulkResult } from '../../models/bulk-import.model';
import { BulkImportResultDialogComponent } from
  '../../components/bulk-import-result-dialog/bulk-import-result-dialog.component';
import { BulkImportErrorService } from './bulk-import-error.service';

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
    private snackbar: MatSnackBar,
    private bulkImportErrorService: BulkImportErrorService
  ) { }

  execute<T>({
    submitFn,
    payload,
    rows,
    dryRun,
    title,
    confirmLabel = 'Import Now',
    columns,
    onPreviewTransform,
    onFinalSuccess,
    onConfirmRetry,
    onErrorsApplied,
    setLoading,
  }: {
    submitFn:
    (
      req: BulkRequest<any>
    ) => Observable<BulkResult<T>>;

    payload: BulkRequest<any>;

    rows: any[];

    dryRun: boolean;

    title: string;

    confirmLabel?: string;

    columns?: {
      key: string;
      label: string;
    }[];

    setLoading?: (
      loading: boolean
    ) => void;

    onPreviewTransform?:
    (
      res: BulkResult<T>
    ) => void;

    onFinalSuccess:
    (
      res: BulkResult<T>
    ) => void;

    onConfirmRetry: () => void;

    onErrorsApplied?:
    (
      res: BulkResult<T>
    ) => void;
  }) {
    setLoading?.(true);

    submitFn(payload)
      .subscribe({

        next: res => {

          setLoading?.(false);

          const dataAsUnknown =
            res.data as unknown[];

          if (
            Array.isArray(
              dataAsUnknown
            )
          ) {

            const previewContainer =
              dataAsUnknown.find(
                hasRowsContainer
              );

            if (
              previewContainer
            ) {

              res = {
                ...res,
                data:
                  previewContainer.rows
              };
            }
          }

          onPreviewTransform?.(
            res
          );

          rows.forEach(
            r =>
              r.patchValue(
                {
                  _error: ''
                },
                {
                  emitEvent: false
                }
              )
          );

          res.errors?.forEach(
            e => {

              const row =
                rows[
                e.row - 1
                ];

              if (row) {

                row.patchValue({
                  _error:
                    e.message
                });
              }
            }
          );

          onErrorsApplied?.(
            res
          );

          if (
            !dryRun &&
            res.failed === 0
          ) {

            this.snackbar.open(
              `${res.success} records imported successfully`,
              'Close',
              {
                duration: 3000
              }
            );

            onFinalSuccess(
              res
            );

            return;
          }

          const ref =
            this.dialog.open(
              BulkImportResultDialogComponent,
              {
                width: '1100px',
                maxWidth: '95vw',
                data: {
                  title,
                  dryRun,
                  result: res,
                  confirmLabel,
                  columns
                }
              }
            );

          ref.afterClosed()
            .subscribe(
              confirm => {

                onErrorsApplied?.(
                  res
                );

                if (
                  confirm
                ) {

                  onConfirmRetry();
                }
              }
            );
        },

        error: err => {

          setLoading?.(false);

          this.bulkImportErrorService
            .handle(err);
        }
      });
  }

  executeMultipart<T>({
    submitFn,
    formData,
    rows,
    title,
    dryRun,
    confirmLabel = 'Import Now',
    columns,
    onPreviewTransform,
    onFinalSuccess,
    onConfirmRetry,
    onErrorsApplied
  }: {
    submitFn: (fd: FormData) => Observable<BulkResult<T>>;
    formData: FormData;
    rows: any[];
    title: string;
    dryRun: boolean;
    confirmLabel?: string;
    columns?: any[];
    onPreviewTransform?: (res: BulkResult<T>) => void;
    onFinalSuccess: (res: BulkResult<T>) => void;
    onConfirmRetry?: () => void;
    onErrorsApplied?: (res: BulkResult<T>) => void;
  }) {

    submitFn(formData).subscribe({
      next: res => {

        onPreviewTransform?.(res);

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

        /* =========================================
          APPLY FIELD-LEVEL ERRORS (LIKE execute)
        ========================================= */

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

          onErrorsApplied?.(res); // restore for navigation

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
