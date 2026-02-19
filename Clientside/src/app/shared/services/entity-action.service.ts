import { Injectable } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { MatSnackBar } from '@angular/material/snack-bar';
import { Observable, of } from 'rxjs';
import { switchMap } from 'rxjs/operators';
import { ReasonDialogComponent } from '../components/reason-dialog/reason-dialog.component';

export interface EntityActionConfig<T> {

  entityName: string;
  displayName: (entity: T) => string;

  disableReasons: string[];
  restoreReasons: string[];
  deleteReasons: string[];

  /* ===== SINGLE ===== */
  disable: (id: string, reason: string | null, extra?: any) => Observable<any>;
  restore: (id: string, reason: string | null, extra?: any) => Observable<any>;
  hardDelete: (id: string, reason: string | null, extra?: any) => Observable<any>;

  /* ===== BULK ===== */
  disableBulk?: (ids: string[], reason: string | null, extra?: any) => Observable<any>;
  restoreBulk?: (ids: string[], reason: string | null, extra?: any) => Observable<any>;
  hardDeleteBulk?: (ids: string[], reason: string | null, extra?: any) => Observable<any>;

  /* ===== OPTIONAL EXTRA PAYLOAD ===== */
  extraPayloadFactory?: (entities: T | T[]) => Observable<any>;

  reload: () => void;
}

@Injectable({ providedIn: 'root' })
export class EntityActionService {

  constructor(
    private dialog: MatDialog,
    private snackbar: MatSnackBar
  ) { }

  /* ============================================================
     INTERNAL EXECUTION WRAPPER
  ============================================================ */

  private execute(
    request$: Observable<any>,
    successMessage: string,
    reload: () => void
  ) {
    request$.subscribe({
      next: () => {
        this.snackbar.open(successMessage, 'Close', { duration: 2500 });
        reload();
      },
      error: (err) => {
        const message =
          err?.error?.message ||
          err?.message ||
          'Operation failed';

        this.snackbar.open(message, 'Close', { duration: 4000 });
      }
    });
  }

  /* ============================================================
     REASON DIALOG (OPTIONAL)
  ============================================================ */

  private openReasonDialog(
    title: string,
    message: string,
    reasons?: string[]
  ): Observable<{ confirmed: boolean; reason: string | null }> {

    if (!reasons || reasons.length === 0) {
      return of({ confirmed: true, reason: null });
    }

    const ref = this.dialog.open(ReasonDialogComponent, {
      width: '420px',
      data: {
        title,
        message,
        confirmText: 'Confirm',
        reasons,
        allowCustomReason: true
      }
    });

    return ref.afterClosed().pipe(
      switchMap(result => {
        if (!result?.confirmed) {
          return of({ confirmed: false, reason: null });
        }

        return of({
          confirmed: true,
          reason: result.reason ?? null
        });
      })
    );
  }

  /* ============================================================
     SINGLE TOGGLE
  ============================================================ */

  toggleSingle<T extends { id: string; deleted?: boolean }>(
    entity: T,
    config: EntityActionConfig<T>
  ) {

    const isDeleted = !!entity.deleted;

    /* ================= DISABLE ================= */

    if (!isDeleted) {

      this.openReasonDialog(
        `Disable ${config.entityName}?`,
        `Provide a reason for disabling ${config.displayName(entity)} (optional).`,
        config.disableReasons
      ).subscribe(result => {

        if (!result.confirmed) return;

        this.execute(
          config.disable(entity.id, result.reason),
          `${config.entityName} disabled`,
          config.reload
        );
      });

      return;
    }

    /* ================= RESTORE ================= */

    this.openReasonDialog(
      `Restore ${config.entityName}?`,
      `Provide a reason for restoring ${config.displayName(entity)} (optional).`,
      config.restoreReasons
    ).subscribe(result => {

      if (!result.confirmed) return;

      this.execute(
        config.restore(entity.id, result.reason),
        `${config.entityName} restored`,
        config.reload
      );
    });
  }

  /* ============================================================
     BULK DISABLE
  ============================================================ */

  bulkDisable<T extends { id: string }>(
    entities: T[],
    config: EntityActionConfig<T>
  ) {

    if (!config.disableBulk) return;

    const ids = entities.map(e => e.id);

    this.openReasonDialog(
      `Disable ${config.entityName}s?`,
      `Provide a reason (optional).`,
      config.disableReasons
    ).subscribe(result => {

      if (!result.confirmed) return;

      this.execute(
        config.disableBulk!(ids, result.reason),
        `${config.entityName}s disabled`,
        config.reload
      );
    });
  }

  /* ============================================================
     BULK RESTORE
  ============================================================ */

  bulkRestore<T extends { id: string }>(
    entities: T[],
    config: EntityActionConfig<T>
  ) {

    if (!config.restoreBulk) return;

    const ids = entities.map(e => e.id);

    this.openReasonDialog(
      `Restore ${config.entityName}s?`,
      `Provide a reason (optional).`,
      config.restoreReasons
    ).subscribe(result => {

      if (!result.confirmed) return;

      this.execute(
        config.restoreBulk!(ids, result.reason),
        `${config.entityName}s restored`,
        config.reload
      );
    });
  }

  /* ============================================================
     BULK HARD DELETE
  ============================================================ */

  bulkHardDelete<T extends { id: string }>(
    entities: T[],
    config: EntityActionConfig<T>
  ) {

    if (!config.hardDeleteBulk) return;

    const ids = entities.map(e => e.id);

    this.openReasonDialog(
      `Delete ${config.entityName}s Permanently?`,
      `You are about to permanently delete ${entities.length} ${config.entityName.toLowerCase()}(s).`,
      config.deleteReasons
    ).subscribe(result => {

      if (!result.confirmed) return;

      this.execute(
        config.hardDeleteBulk!(ids, result.reason),
        `${config.entityName}s permanently deleted`,
        config.reload
      );
    });
  }
}