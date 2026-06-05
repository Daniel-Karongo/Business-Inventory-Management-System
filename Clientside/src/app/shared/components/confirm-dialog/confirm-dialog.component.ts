import {
  Component,
  Inject
} from '@angular/core';

import {
  MAT_DIALOG_DATA,
  MatDialogRef,
  MatDialogModule
} from '@angular/material/dialog';

import { CommonModule } from '@angular/common';

import { MatButtonModule } from '@angular/material/button';

import { FormsModule } from '@angular/forms';

import { MatCheckboxModule } from '@angular/material/checkbox';

import { MatIconModule } from '@angular/material/icon';

import { MatRadioModule } from '@angular/material/radio';

export interface ConfirmDialogOption {

  value: string;

  label: string;

  description?: string;

  deletes?: string[];

  keeps?: string[];

  warning?: string;

  /**
   * Optional
   * Enables expand/collapse behavior.
   */
  collapsible?: boolean;

  /**
   * Optional
   * Initial state when dialog loads.
   */
  initiallyExpanded?: boolean;
}

export interface ConfirmDialogData {

  title: string;

  message: string;

  confirmText?: string;

  cancelText?: string;

  severity?: 'info' | 'warning' | 'danger';

  details?: string[];

  options?: ConfirmDialogOption[];

  selectedOption?: string;

  requireAcknowledgement?: boolean;

  acknowledgementText?: string;
}

@Component({
  selector: 'app-confirm-dialog',

  standalone: true,

  imports: [
    CommonModule,
    FormsModule,
    MatButtonModule,
    MatDialogModule,
    MatCheckboxModule,
    MatIconModule,
    MatRadioModule
  ],

  templateUrl: './confirm-dialog.component.html',

  styleUrls: ['./confirm-dialog.component.scss']
})
export class ConfirmDialogComponent {

  selectedOption?: string;

  acknowledged = false;

  /**
   * Tracks expanded option cards.
   */
  expandedOptions = new Set<string>();

  constructor(
    private ref: MatDialogRef<ConfirmDialogComponent>,

    @Inject(MAT_DIALOG_DATA)
    public data: ConfirmDialogData
  ) {

    this.selectedOption =
      data.selectedOption;

    this.initializeExpansion();
  }

  /* =========================================================
     INIT
  ========================================================= */

  private initializeExpansion(): void {

    if (!this.data.options?.length) {
      return;
    }

    for (const option of this.data.options) {

      if (
        option.collapsible &&
        option.initiallyExpanded
      ) {
        this.expandedOptions.add(
          option.value
        );
      }
    }

    if (
      this.selectedOption &&
      this.data.options.some(
        o =>
          o.value === this.selectedOption &&
          o.collapsible
      )
    ) {
      this.expandedOptions.add(
        this.selectedOption
      );
    }
  }

  /* =========================================================
     OPTION SELECTION
  ========================================================= */

  selectOption(
    value: string
  ): void {

    this.selectedOption = value;

    const option =
      this.data.options?.find(
        o => o.value === value
      );

    if (
      option?.collapsible
    ) {

      this.expandedOptions.clear();

      this.expandedOptions.add(
        value
      );
    }
  }

  /* =========================================================
     EXPANSION
  ========================================================= */

  toggleDetails(
    value: string
  ): void {

    if (
      this.expandedOptions.has(value)
    ) {

      this.expandedOptions.delete(
        value
      );

    } else {

      this.expandedOptions.add(
        value
      );
    }
  }

  isExpanded(
    value: string
  ): boolean {

    return this.expandedOptions.has(
      value
    );
  }

  /* =========================================================
     ACTIONS
  ========================================================= */

  confirm(): void {

    this.ref.close({
      confirmed: true,
      option: this.selectedOption
    });
  }

  cancel(): void {

    this.ref.close({
      confirmed: false
    });
  }

  /* =========================================================
     VALIDATION
  ========================================================= */

  canConfirm(): boolean {

    if (
      this.data.requireAcknowledgement &&
      !this.acknowledged
    ) {
      return false;
    }

    if (
      this.data.options?.length &&
      !this.selectedOption
    ) {
      return false;
    }

    return true;
  }

  getDeleteCount(
    option: ConfirmDialogOption
  ): number {
    return option.deletes?.length ?? 0;
  }

  getKeepCount(
    option: ConfirmDialogOption
  ): number {
    return option.keeps?.length ?? 0;
  }
}