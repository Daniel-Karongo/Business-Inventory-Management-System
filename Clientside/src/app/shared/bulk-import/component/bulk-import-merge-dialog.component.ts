import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatDialogRef, MatDialogModule } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';

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

    .merge-text {
      font-size: var(--text-sm);
      color: var(--text-secondary);

      p {
        margin: 0 0 8px;
      }

      ul {
        padding-left: 18px;
        margin: 6px 0 0;
      }
    }
  `,
  template: `
    <h2 mat-dialog-title class="dialog-title">Existing import detected</h2>

    <mat-dialog-content class="merge-text">
      <p>
        There are already imported lines in this form.
      </p>

      <p>
        Choose how you would like to proceed:
      </p>

      <ul>
        <li>
          <strong>Add to existing</strong>
          — keep current lines and append new ones
        </li>
        <li>
          <strong>Replace all</strong>
          — remove existing lines and import fresh data
        </li>
      </ul>
    </mat-dialog-content>

    <mat-dialog-actions align="end">
      <button
        mat-stroked-button
        (click)="replace()">
        Replace all
      </button>

      <button
        mat-flat-button
        color="primary"
        (click)="append()">
        Add to existing
      </button>
    </mat-dialog-actions>
  `
})
export class BulkImportMergeDialogComponent {

  constructor(
    private ref: MatDialogRef<BulkImportMergeDialogComponent>
  ) {}

  append() {
    this.ref.close('append');
  }

  replace() {
    this.ref.close('replace');
  }
}