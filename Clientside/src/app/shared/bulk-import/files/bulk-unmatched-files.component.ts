import { Component, Input, Output, EventEmitter } from '@angular/core';
import { CommonModule } from '@angular/common';
import { BulkAssignedFile } from './bulk-file.model';
import { MatButtonModule } from '@angular/material/button';
import { MatSelectModule } from '@angular/material/select';

@Component({
  standalone: true,
  selector: 'app-bulk-unmatched-files',
  imports: [
    CommonModule,
    MatButtonModule,
    MatSelectModule
  ],
  template: `
    <div *ngIf="files.length" class="unmatched-panel">

      <h4>Unmatched Files</h4>

      <div *ngFor="let file of files" class="unmatched-item">

        <span>{{ file.file.name }}</span>

        <mat-select
          placeholder="Assign to row"
          (selectionChange)="assign.emit({ file, rowIndex: $event.value })">

          <mat-option
            *ngFor="let row of rowNames; let i = index"
            [value]="i">

            {{ row }}

          </mat-option>

        </mat-select>

      </div>
    </div>
  `,
  styles: [`
    .unmatched-panel {
      border: 1px solid var(--border);
      padding: 16px;
      border-radius: 10px;
      background: var(--surface);
    }

    .unmatched-item {
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 12px;
    }
  `]
})
export class BulkUnmatchedFilesComponent {

  @Input() files: BulkAssignedFile[] = [];
  @Input() rowNames: string[] = [];

  @Output() assign = new EventEmitter<{
    file: BulkAssignedFile,
    rowIndex: number
  }>();
}