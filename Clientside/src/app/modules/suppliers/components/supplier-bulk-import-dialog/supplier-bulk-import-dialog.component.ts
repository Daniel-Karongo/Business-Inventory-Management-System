import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import {
  FormArray,
  FormBuilder,
  FormGroup,
  Validators,
  ReactiveFormsModule
} from '@angular/forms';

import * as ExcelJS from 'exceljs';
import * as XLSX from 'xlsx';
import { saveAs } from 'file-saver';

import { MatDialog, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatFormFieldModule } from '@angular/material/form-field';

import { forkJoin } from 'rxjs';

import { SupplierService } from '../../services/supplier.service';
import { CategoryService } from '../../../categories/services/category.service';
import { BulkRequest, BulkResult } from '../../../../shared/models/bulk-import.model';
import { BulkImportResultDialogComponent } from '../../../../shared/components/bulk-import-result-dialog/bulk-import-result-dialog.component';
import { Supplier } from '../../models/supplier.model';

@Component({
  standalone: true,
  selector: 'app-supplier-bulk-import-dialog',
  templateUrl: './supplier-bulk-import-dialog.component.html',
  styleUrls: ['./supplier-bulk-import-dialog.component.scss'],
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatDialogModule,
    MatButtonModule,
    MatInputModule,
    MatIconModule,
    MatCheckboxModule,
    MatFormFieldModule,
    MatSnackBarModule
  ]
})
export class SupplierBulkImportDialogComponent implements OnInit {

  form!: FormGroup;
  submitting = false;
  categories: any[] = [];

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<SupplierBulkImportDialogComponent>,
    private supplierService: SupplierService,
    private categoryService: CategoryService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {
    this.form = this.fb.group({
      dryRun: [true],
      rows: this.fb.array([])
    });

    this.loadCategories();
    this.addRow();
  }

  get rows(): FormArray {
    return this.form.get('rows') as FormArray;
  }

  private createRow(data?: any): FormGroup {
    return this.fb.group({
      name: [data?.name ?? '', Validators.required],
      emails: [data?.emails ?? '', Validators.required],
      phones: [data?.phones ?? '', Validators.required],
      address: [data?.address ?? ''],
      region: [data?.region ?? ''],
      categories: [data?.categories ?? ''],
      _error: ['']
    });
  }

  addRow(data?: any) {
    this.rows.push(this.createRow(data));
  }

  removeRow(i: number) {
    this.rows.removeAt(i);
  }

  private loadCategories() {
    this.categoryService.getAll('flat', false)
      .subscribe(c => this.categories = c || []);
  }

  /* =========================
     CSV
     ========================= */

  downloadCsvTemplate() {
    const csv =
      `name,emails,phones,address,region,categories
Acme Ltd,info@acme.com,0712345678,Nairobi,Nairobi,Electronics,Accessories`;

    saveAs(new Blob([csv], { type: 'text/csv' }), 'suppliers-bulk-template.csv');
  }

  async downloadExcelTemplate() {

    const workbook = new ExcelJS.Workbook();
    const worksheet = workbook.addWorksheet('Suppliers', {
      views: [{ state: 'frozen', ySplit: 1 }]
    });

    /* ============================
       DEFINE COLUMNS
       ============================ */
    worksheet.columns = [
      { header: 'name', key: 'name', width: 26 },
      { header: 'emails', key: 'emails', width: 32 },
      { header: 'phones', key: 'phones', width: 22 },
      { header: 'address', key: 'address', width: 26 },
      { header: 'region', key: 'region', width: 18 },
      { header: 'categories', key: 'categories', width: 32 }
    ];

    /* ============================
       HEADER STYLING
       ============================ */
    const headerRow = worksheet.getRow(1);
    headerRow.font = { bold: true };

    headerRow.eachCell(cell => {
      cell.alignment = { vertical: 'middle', horizontal: 'left' };
      cell.border = {
        top: { style: 'thin' },
        left: { style: 'thin' },
        bottom: { style: 'thin' },
        right: { style: 'thin' }
      };
    });

    /* ============================
       EXAMPLE ROW
       ============================ */
    const exampleRow = worksheet.addRow({
      name: 'Acme Ltd',
      emails: 'info@acme.com',
      phones: '0712345678',
      address: 'Nairobi',
      region: 'Nairobi',
      categories: 'Electronics,Accessories'
    });

    exampleRow.eachCell(cell => {
      cell.alignment = { vertical: 'middle', horizontal: 'left' };
      cell.border = {
        top: { style: 'thin' },
        left: { style: 'thin' },
        bottom: { style: 'thin' },
        right: { style: 'thin' }
      };
    });

    // 🔥 FORCE PHONES COLUMN TO TEXT
    exampleRow.getCell('phones').numFmt = '@';

    /* ============================
       EMPTY DATA ROWS
       ============================ */
    const ROW_COUNT = 200;

    for (let i = 0; i < ROW_COUNT; i++) {
      const row = worksheet.addRow({
        name: '',
        emails: '',
        phones: '',
        address: '',
        region: '',
        categories: ''
      });

      row.eachCell(cell => {
        cell.alignment = { vertical: 'middle', horizontal: 'left' };
        cell.border = {
          top: { style: 'thin' },
          left: { style: 'thin' },
          bottom: { style: 'thin' },
          right: { style: 'thin' }
        };
      });

      // 🔥 FORCE PHONES COLUMN TO TEXT (ALL ROWS)
      row.getCell('phones').numFmt = '@';
    }

    /* ============================
       FINALIZE
       ============================ */
    const buffer = await workbook.xlsx.writeBuffer();

    saveAs(
      new Blob([buffer], {
        type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
      }),
      'suppliers-bulk-template.xlsx'
    );
  }

  importCsv(event: Event) {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;

    const reader = new FileReader();
    reader.onload = () => this.parseCsv(reader.result as string);
    reader.readAsText(input.files[0]);
  }

  private parseCsv(text: string) {
    const lines = text.split('\n').slice(1); // skip header
    this.rows.clear();

    for (const line of lines) {
      if (!line.trim()) continue;

      const [
        name,
        emails,
        phones,
        address,
        region,
        categories
      ] = line.split(',');

      this.addRow({
        name: name?.trim(),
        emails: emails?.trim(),
        phones: phones?.trim(),
        address: address?.trim(),
        region: region?.trim(),
        categories: categories?.trim()
      });
    }
  }

  /* =========================
   EXCEL
   ========================= */

  importExcel(event: Event) {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;

    const reader = new FileReader();

    reader.onload = e => {
      const binary = e.target?.result;
      if (!binary) return;

      const workbook = XLSX.read(binary, { type: 'binary' });
      const sheetName = workbook.SheetNames[0];
      const sheet = workbook.Sheets[sheetName];

      // Convert sheet → objects (one per physical row)
      const rawRows = XLSX.utils.sheet_to_json<any>(sheet, {
        defval: ''
      });

      // Clear existing form rows
      this.rows.clear();

      // 🔥 FILTER: only rows with real data
      rawRows
        .filter(r =>
          String(r.name || '').trim() ||
          String(r.emails || '').trim() ||
          String(r.phones || '').trim()
        )
        .forEach(r => {
          this.addRow({
            name: String(r.name || '').trim(),
            emails: String(r.emails || '').trim(),
            phones: String(r.phones || '').trim(),
            address: String(r.address || '').trim(),
            region: String(r.region || '').trim(),
            categories: String(r.categories || '').trim()
          });
        });
    };

    reader.readAsBinaryString(input.files[0]);
  }

  /* =========================
     SUBMIT
     ========================= */

  submit() {
    if (this.form.invalid) return;

    this.submitting = true;

    const payload: BulkRequest<any> = {
      items: this.rows.controls.map(r => ({
        name: r.value.name,
        email: [r.value.emails],
        phoneNumber: [r.value.phones],
        address: r.value.address,
        region: r.value.region,
        categoryNames: r.value.categories
          ? r.value.categories.split(',').map((c: string) => c.trim())
          : []
      })),
      options: {
        dryRun: this.form.value.dryRun,
        skipDuplicates: true
      }
    };

    this.supplierService.bulkImport(payload).subscribe({
      next: (res: BulkResult<Supplier>) => {

        this.rows.controls.forEach(r =>
          r.patchValue({ _error: '' }, { emitEvent: false })
        );

        res.errors?.forEach(e => {
          this.rows.at(e.row - 1)?.patchValue({ _error: e.message });
        });

        if (!this.form.value.dryRun && res.failed === 0) {
          this.snackbar.open(
            `${res.success} suppliers imported successfully`,
            'Close',
            { duration: 3000 }
          );
          this.dialogRef.close(true);
          return;
        }

        this.dialog.open(BulkImportResultDialogComponent, {
          width: '1100px',
          maxWidth: '95vw',
          data: {
            title: 'Supplier Import Preview',
            dryRun: this.form.value.dryRun,
            result: res,
            confirmLabel: 'Import Now',
            columns: [
              { key: 'name', label: 'Name' },
              { key: 'email', label: 'Emails' },
              { key: 'phoneNumber', label: 'Phones' },
              { key: 'region', label: 'Region' }
            ]
          }
        }).afterClosed().subscribe(confirm => {
          if (confirm) {
            this.form.patchValue({ dryRun: false });
            this.submit();
          }
        });

        this.submitting = false;
      },
      error: () => {
        this.submitting = false;
        this.snackbar.open(
          'Supplier bulk import failed',
          'Close',
          { duration: 3000 }
        );
      }
    });
  }

  close() {
    this.dialogRef.close();
  }
}