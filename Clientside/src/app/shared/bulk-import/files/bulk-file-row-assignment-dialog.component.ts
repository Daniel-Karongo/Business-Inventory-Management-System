import { Component, Inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import {
  MAT_DIALOG_DATA,
  MatDialogModule,
  MatDialogRef
} from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatInputModule } from '@angular/material/input';
import { MatFormFieldModule } from '@angular/material/form-field';

import { BulkAssignedFile } from './bulk-file.model';

interface DialogData {
  files: BulkAssignedFile[];
  rows: { index: number; name: string }[];
}

@Component({
  standalone: true,
  selector: 'app-bulk-file-row-assignment-dialog',
  imports: [
    CommonModule,
    FormsModule,
    MatDialogModule,
    MatButtonModule,
    MatCheckboxModule,
    MatInputModule,
    MatFormFieldModule
  ],
  template: `
    <h2 mat-dialog-title>Assign File</h2>

    <mat-dialog-content class="content">

      <div class="file-name">
        <strong>
          {{ data.files.length === 1
              ? data.files[0].file.name
              : data.files.length + ' files selected'
          }}
        </strong>
      </div>

      <mat-form-field appearance="fill">
        <mat-label>Search rows</mat-label>
        <input matInput [(ngModel)]="search" />
      </mat-form-field>

      <div class="row-list">

        <div *ngIf="assignedRows().length">
          <h4>Assigned</h4>

          <div *ngFor="let row of assignedRows()" class="row-item">
            <mat-checkbox
              [checked]="true"
              (change)="toggle(row.index, $event.checked)">
              {{ row.name || ('Row ' + (row.index + 1)) }}
            </mat-checkbox>
          </div>
        </div>

        <div *ngIf="unassignedRows().length">
          <h4>Unassigned</h4>

          <div *ngFor="let row of unassignedRows()" class="row-item">
            <mat-checkbox
              [checked]="false"
              (change)="toggle(row.index, $event.checked)">
              {{ row.name || ('Row ' + (row.index + 1)) }}
            </mat-checkbox>
          </div>
        </div>

      </div>


    </mat-dialog-content>

    <mat-dialog-actions align="end">
      <button mat-button (click)="close()">Cancel</button>
      <button mat-flat-button color="primary" (click)="save()">Save</button>
    </mat-dialog-actions>
  `,
  styles: [`
    .content {
      display: flex;
      flex-direction: column;
      gap: 16px;
      width: 500px;
      max-height: 70vh;
      overflow: hidden;
    }

    .row-list {
      overflow: auto;
      border: 1px solid var(--border);
      border-radius: 8px;
      padding: 8px;
      max-height: 400px;
    }

    .row-item {
      padding: 4px 0;
    }

    .file-name {
      font-size: 0.9rem;
      opacity: 0.8;
    }
  `]
})
export class BulkFileRowAssignmentDialogComponent {

  search = '';

  selected = new Set<number>();

  constructor(
    @Inject(MAT_DIALOG_DATA) public data: DialogData,
    private dialogRef: MatDialogRef<BulkFileRowAssignmentDialogComponent>
  ) {
    data.files[0]?.assignedRowIndexes
      ?.forEach(i => this.selected.add(i));
  }

  private baseFiltered() {

    if (!this.search) return this.data.rows;

    return this.data.rows.filter(r =>
      (r.name || '')
        .toLowerCase()
        .includes(this.search.toLowerCase())
    );
  }

  assignedRows() {
    return this.baseFiltered()
      .filter(r => this.selected.has(r.index));
  }

  unassignedRows() {
    return this.baseFiltered()
      .filter(r => !this.selected.has(r.index));
  }


  toggle(index: number, checked: boolean) {
    if (checked) {
      this.selected.add(index);
    } else {
      this.selected.delete(index);
    }
  }

  save() {
    this.dialogRef.close([...this.selected]);
  }

  close() {
    this.dialogRef.close(null);
  }
}