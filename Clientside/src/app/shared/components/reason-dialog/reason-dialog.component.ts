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
  action: 'DISABLE' | 'RESTORE' | 'DELETE';
  confirmText?: string;
  cancelText?: string;
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

  disableReasons = [
    'Security risk', 'Policy violation', 'Left company', 'Temporary suspension',
    'Incorrect details', 'Duplicate account'
  ];

  restoreReasons = [
    'Mistaken disable', 'Issue resolved', 'Reinstated by management',
    'Information updated', 'Investigation complete'
  ];

  deleteReasons = [
    'GDPR / Data cleanup',
    'Requested by management',
    'Duplicate account',
    'Never used',
    'Fraudulent account'
  ];

  selectedReason: string | null = null;
  customReason = '';

  constructor(
    private ref: MatDialogRef<ReasonDialogComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ReasonDialogData
  ) {}

  get reasonOptions(): string[] {
    switch (this.data.action) {
      case 'RESTORE': return this.restoreReasons;
      case 'DELETE': return this.deleteReasons;
      default: return this.disableReasons;
    }
  }

  confirm() {
    let reason = this.selectedReason === 'OTHER'
      ? (this.customReason.trim() || null)
      : this.selectedReason;

    this.ref.close({ confirmed: true, reason });
  }

  cancel() {
    this.ref.close({ confirmed: false });
  }
}