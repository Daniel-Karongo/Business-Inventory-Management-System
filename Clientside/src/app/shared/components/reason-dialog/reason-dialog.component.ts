import { Component, Inject } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef, MatDialogModule } from '@angular/material/dialog';
import { CommonModule } from '@angular/common';
import { MatButtonModule } from '@angular/material/button';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { FormsModule } from '@angular/forms';
import { MatSelectModule } from '@angular/material/select';

export interface ReasonDialogData {
  title: string;
  message: string;
  action?: 'DISABLE' | 'RESTORE' | 'DELETE';
  confirmText?: string;
  cancelText?: string;

  reasons?: string[];
  allowCustomReason?: boolean;
}

@Component({
  selector: 'app-reason-dialog',
  standalone: true,
  templateUrl: './reason-dialog.component.html',
  styleUrls: ['./reason-dialog.component.scss'],
  imports: [
    CommonModule,
    FormsModule,
    MatDialogModule,
    MatButtonModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule
  ]
})
export class ReasonDialogComponent {

  selectedReason: string | null = null;
  customReason = '';

  constructor(
    private ref: MatDialogRef<ReasonDialogComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ReasonDialogData
  ) { }

  get reasonOptions(): string[] {
    return this.data.reasons ?? [];
  }

  get allowCustom(): boolean {
    return this.data.allowCustomReason !== false;
  }

  confirm() {

    let reason: string | null = null;

    if (this.selectedReason === 'OTHER') {
      reason = this.customReason.trim() || null;
    } else {
      reason = this.selectedReason ?? null;
    }

    this.ref.close({ confirmed: true, reason });
  }

  cancel() {
    this.ref.close({ confirmed: false });
  }
}