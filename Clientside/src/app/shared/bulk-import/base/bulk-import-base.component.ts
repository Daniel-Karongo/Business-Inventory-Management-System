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

  /* 🔴 DECLARE CONTRACT */
  protected abstract get errorRows(): number[];

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

  goToNextError() {
    const errors = this.errorRows;
    if (!errors.length) return;

    const current = this.scroll.getCurrentLine();

    const next =
      errors.find(r => r > current) ??
      errors[0];

    this.scroll.goToLine(next);
  }

  goToPreviousError() {
    const errors = this.errorRows;
    if (!errors.length) return;

    const current = this.scroll.getCurrentLine();

    const prev =
      [...errors].reverse().find(r => r < current) ??
      errors[errors.length - 1];

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