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
  private currentErrorIndex = -1;

  /* 🔴 DECLARE CONTRACT */
  protected abstract get errorRows(): number[];

  protected navigateToErrorRow(
    line: number
  ): void {

    this.scroll.goToLine(line);

  }

  protected notifyNoMoreErrors(): void {

    this.snackbar.open(
      'No more errors',
      'Close',
      {
        duration: 3000
      }
    );

  }

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

    const errors =
      this.errorRows;

    if (!errors.length) {

      this.notifyNoMoreErrors();

      return;

    }

    // move forward
    this.currentErrorIndex++;

    // reached end
    if (
      this.currentErrorIndex >=
      errors.length
    ) {

      this.currentErrorIndex =
        errors.length - 1;

      this.notifyNoMoreErrors();

      return;

    }

    this.navigateToErrorRow(
      errors[this.currentErrorIndex]
    );

  }

  goToPreviousError() {

    const errors =
      this.errorRows;

    if (!errors.length) {

      this.notifyNoMoreErrors();

      return;

    }

    // move backward
    this.currentErrorIndex--;

    // reached start
    if (
      this.currentErrorIndex < 0
    ) {

      this.currentErrorIndex = 0;

      this.notifyNoMoreErrors();

      return;

    }

    this.navigateToErrorRow(
      errors[this.currentErrorIndex]
    );

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

    this.snackbar.open(
      msg,
      'Close',
      {
        duration: 7000,
        horizontalPosition: 'center',
        verticalPosition: 'bottom'
      }
    );

  }

  protected notifyError(msg: string) {
    this.snackbar.open(
      msg,
      'Close',
      {
        duration: 7000,
        horizontalPosition: 'center',
        verticalPosition: 'bottom'
      }
    );
  }
}
