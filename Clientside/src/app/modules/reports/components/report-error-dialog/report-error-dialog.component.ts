import { Component, Inject } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogModule } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { CommonModule } from '@angular/common';

@Component({
  standalone: true,
  imports: [CommonModule, MatDialogModule, MatButtonModule],
  template: `
    <h2 mat-dialog-title>Report error</h2>

    <mat-dialog-content class="error-content">
      {{ data.message }}
    </mat-dialog-content>

    <mat-dialog-actions align="end">
      <button mat-flat-button color="primary" mat-dialog-close>
        OK
      </button>
    </mat-dialog-actions>
  `,
  styles: [`
    .error-content {
      color: #b00020;
      font-size: 14px;
      line-height: 1.5;
      margin-top: 8px;
    }
  `]
})
export class ReportErrorDialogComponent {
  constructor(@Inject(MAT_DIALOG_DATA) public data: { message: string }) {}
}