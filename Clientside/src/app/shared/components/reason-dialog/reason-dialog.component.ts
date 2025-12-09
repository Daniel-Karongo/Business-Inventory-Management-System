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
  predefinedReasons = [
    'Security risk',
    'Policy violation',
    'Left company',
    'Temporary suspension',
    'Incorrect details',
    'Duplicate account'
  ];

  selectedReason: string | null = null;
  customReason: string = '';

  constructor(
    private ref: MatDialogRef<ReasonDialogComponent>,
    @Inject(MAT_DIALOG_DATA) public data: ReasonDialogData
  ) {}

  confirm() {
    let reason: string | null = null;

    if (this.selectedReason && this.selectedReason !== 'OTHER') {
      reason = this.selectedReason;
    } else if (this.selectedReason === 'OTHER' && this.customReason.trim()) {
      reason = this.customReason.trim();
    }

    this.ref.close({ confirmed: true, reason });
  }

  cancel() {
    this.ref.close({ confirmed: false });
  }
}