import {
  Component,
  Inject,
  AfterViewInit,
  ViewChild
} from '@angular/core';
import { CommonModule } from '@angular/common';

import {
  MAT_DIALOG_DATA,
  MatDialogModule,
  MatDialogRef
} from '@angular/material/dialog';

import { MatButtonModule } from '@angular/material/button';
import { MatTableModule, MatTableDataSource } from '@angular/material/table';
import { MatTabsModule } from '@angular/material/tabs';
import { MatIconModule } from '@angular/material/icon';
import { MatPaginator, MatPaginatorModule } from '@angular/material/paginator';
import { BulkImportResultDialogData } from '../../models/bulk-import.model';

@Component({
  standalone: true,
  selector: 'app-bulk-import-result-dialog',
  templateUrl: './bulk-import-result-dialog.component.html',
  styleUrls: ['./bulk-import-result-dialog.component.scss'],
  imports: [
    CommonModule,
    MatDialogModule,
    MatButtonModule,
    MatTableModule,
    MatTabsModule,
    MatIconModule,
    MatPaginatorModule
  ]
})
export class BulkImportResultDialogComponent implements AfterViewInit {

  /** SUCCESS table */
  successDS: MatTableDataSource<any>;
  displayedColumns: string[];

  /** FAILED table */
  failedDS: MatTableDataSource<any>;

  @ViewChild('successPaginator') successPaginator!: MatPaginator;
  @ViewChild('failedPaginator') failedPaginator!: MatPaginator;

  constructor(
    @Inject(MAT_DIALOG_DATA)
    public data: BulkImportResultDialogData,
    private dialogRef: MatDialogRef<BulkImportResultDialogComponent>
  ) {
    this.displayedColumns = data.columns.map(c => c.key);

    this.successDS = new MatTableDataSource(data.result.data);
    this.failedDS = new MatTableDataSource(data.result.errors);
  }

  ngAfterViewInit(): void {
    this.successDS.paginator = this.successPaginator;
    this.failedDS.paginator = this.failedPaginator;
  }

  value(row: any, key: string): any {
    return row?.[key];
  }

  close() {
    this.dialogRef.close(false);
  }

  confirm() {
    this.dialogRef.close(true);
  }
}