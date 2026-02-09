import {
  Directive,
  AfterViewInit,
  OnDestroy,
  inject
} from '@angular/core';
import { MatSnackBar } from '@angular/material/snack-bar';
import { BulkResult } from '../../models/bulk-import.model';
import { BulkImportScrollService } from './bulk-import-scroll.service';

@Directive()
export abstract class BulkImportBaseComponent<T>
  implements AfterViewInit, OnDestroy {

  protected snackbar = inject(MatSnackBar);
  protected scroll = inject(BulkImportScrollService);

  submitting = false;

  protected errorRows: number[] = [];
  protected errorIndex = 0;

  /* ================= LIFECYCLE ================= */

  ngAfterViewInit(): void {
    window.addEventListener('keydown', this.handleKeyNav);
  }

  ngOnDestroy(): void {
    window.removeEventListener('keydown', this.handleKeyNav);
  }

  /* ================= KEYBOARD ================= */

  protected handleKeyNav = (e: KeyboardEvent) => {
    if (e.ctrlKey && e.key.toLowerCase() === 'g') {
      e.preventDefault();
      document
        .querySelector<HTMLInputElement>('#goToLineInput')
        ?.focus();
    }

    if (e.key === 'Enter') {
      const el = document.activeElement as HTMLInputElement;
      if (el?.id === 'goToLineInput') {
        e.preventDefault();
        const line = el.valueAsNumber;
        if (line) this.goToLine(line);
      }
    }
  };

  /* ================= ERRORS ================= */

  protected cacheErrors(res: BulkResult<any>) {
    this.errorRows = res.errors?.map(e => e.row).sort((a, b) => a - b) || [];

    if (this.errorRows.length) {
      queueMicrotask(() =>
        this.scroll.goToLine(this.errorRows[0])
      );
    }
  }

  goToNextError() {
    if (!this.errorRows.length) return;

    const current = this.scroll.getCurrentLine();

    const next =
      this.errorRows.find(r => r > current) ??
      this.errorRows[0]; // wrap around

    this.scroll.goToLine(next);
  }

  goToPreviousError() {
    if (!this.errorRows.length) return;

    const current = this.scroll.getCurrentLine();

    const prev =
      [...this.errorRows].reverse().find(r => r < current) ??
      this.errorRows[this.errorRows.length - 1]; // wrap around

    this.scroll.goToLine(prev);
  }

  /* ================= SCROLL ================= */

  scrollToTop() {
    this.scroll.scrollTop();
  }

  scrollToBottom() {
    this.scroll.scrollBottom();
  }

  goToLine(line: number) {
    this.scroll.goToLine(line);
  }

  /* ================= NOTIFY ================= */

  protected notifySuccess(msg: string) {
    this.snackbar.open(msg, 'Close', { duration: 3000 });
  }

  protected notifyError(msg: string) {
    this.snackbar.open(msg, 'Close', { duration: 3000 });
  }
}