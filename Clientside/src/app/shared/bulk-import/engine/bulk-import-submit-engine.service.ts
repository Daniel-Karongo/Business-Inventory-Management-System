import { Injectable } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { MatSnackBar } from '@angular/material/snack-bar';
import { Observable } from 'rxjs';

import { BulkRequest, BulkResult } from '../../models/bulk-import.model';
import { BulkImportResultDialogComponent } from
  '../../components/bulk-import-result-dialog/bulk-import-result-dialog.component';

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

        if (!dryRun && res.failed === 0) {
          this.snackbar.open(
            `${res.success} records imported successfully`,
            'Close',
            { duration: 3000 }
          );
          onFinalSuccess(res);
          return;
        }

        const ref = this.dialog.open(BulkImportResultDialogComponent, {
          width: '1100px',
          maxWidth: '95vw',
          data: { title, dryRun, result: res, confirmLabel, columns }
        });

        ref.afterClosed().subscribe(confirm => {
          // 🔴 IMPORTANT: restore error navigation after dialog
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
}