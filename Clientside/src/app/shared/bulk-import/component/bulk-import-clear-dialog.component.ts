import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { MatButtonModule } from '@angular/material/button';
import { MatDialogModule, MatDialogRef } from '@angular/material/dialog';

@Component({
  standalone: true,
  imports: [
    CommonModule,
    MatDialogModule,
    MatButtonModule
  ],
  styles: `
    .dialog-title {
      font-weight: 600;
    }
  `,
  template: `
    <h2 mat-dialog-title class="dialog-title">Clear all lines</h2>

    <mat-dialog-content>
      This will remove all imported lines from the form.
      <br /><br />
      This action cannot be undone.
    </mat-dialog-content>

    <mat-dialog-actions align="end">
      <button mat-button (click)="close(false)">
        Cancel
      </button>

      <button
        mat-flat-button
        color="warn"
        (click)="close(true)">
        Clear all
      </button>
    </mat-dialog-actions>
  `
})
export class BulkImportClearDialogComponent {

  constructor(
    private ref: MatDialogRef<BulkImportClearDialogComponent>
  ) { }

  close(result: boolean) {
    this.ref.close(result);
  }
}