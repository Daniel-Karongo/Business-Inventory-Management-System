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
import { MatSelectModule } from '@angular/material/select';

import { BulkRequest, BulkResult } from '../../../../shared/models/bulk-import.model';
import { BulkImportResultDialogComponent } from '../../../../shared/components/bulk-import-result-dialog/bulk-import-result-dialog.component';
import { SalesService } from '../../services/sales.service';
import { MatDatepickerModule } from '@angular/material/datepicker';

@Component({
  standalone: true,
  selector: 'app-sale-bulk-import-dialog',
  templateUrl: './sale-bulk-import-dialog.component.html',
  styleUrls: ['./sale-bulk-import-dialog.component.scss'],
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatDialogModule,
    MatButtonModule,
    MatInputModule,
    MatIconModule,
    MatCheckboxModule,
    MatFormFieldModule,
    MatSelectModule,
    MatSnackBarModule,
    MatDatepickerModule
  ]
})
export class SaleBulkImportDialogComponent implements OnInit {

  form!: FormGroup;
  submitting = false;

  /** defaults must match backend */
  readonly DEFAULT_VARIANT = 'STANDARD';
  readonly DEFAULT_BRANCH = 'MAIN';

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<SaleBulkImportDialogComponent>,
    private salesService: SalesService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {
    this.form = this.fb.group({
      dryRun: [true],
      mode: ['OPERATIONAL', Validators.required],
      rows: this.fb.array([])
    });

    this.addRow();
  }

  /* =========================
     FORM HELPERS
     ========================= */

  get rows(): FormArray {
    return this.form.get('rows') as FormArray;
  }

  private createRow(data?: any): FormGroup {
    return this.fb.group({
      receiptNo: [data?.receiptNo ?? '', Validators.required],
      sku: [data?.sku ?? ''],
      productName: [data?.productName ?? ''],
      variant: [data?.variant ?? this.DEFAULT_VARIANT],
      quantity: [data?.quantity ?? 1, [Validators.required, Validators.min(1)]],
      unitPrice: [data?.unitPrice ?? null, [Validators.required, Validators.min(0)]],
      branchCode: [data?.branchCode ?? this.DEFAULT_BRANCH],
      saleDate: [
        data?.saleDate instanceof Date
          ? data.saleDate
          : data?.saleDate
            ? new Date(data.saleDate)
            : new Date()
      ],
      payments: [data?.payments ?? ''],
      _error: ['']
    });
  }

  addRow(data?: any) {
    this.rows.push(this.createRow(data));
  }

  removeRow(index: number) {
    this.rows.removeAt(index);
  }

  /* =========================
     CSV
     ========================= */

  downloadCsvTemplate() {
    const csv =
      `receiptNo,sku,productName,variant,quantity,unitPrice,branchCode,saleDate,payments
R-001,SKU-001,,STANDARD,2,4500,MAIN,,CASH|9000
R-001,SKU-002,,STANDARD,1,1200,MAIN,,
R-002,,Lenovo ThinkPad T480,STANDARD,1,42360,MAIN,,MPESA|42360|ABC123`;

    saveAs(new Blob([csv], { type: 'text/csv' }), 'sales-bulk-template.csv');
  }

  importCsv(event: Event) {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;

    const reader = new FileReader();
    reader.onload = () => this.parseCsv(reader.result as string);
    reader.readAsText(input.files[0]);
  }

  private parseCsv(text: string) {
    const lines = text.split('\n').slice(1);
    this.rows.clear();

    for (const line of lines) {
      if (!line.trim()) continue;

      const [
        receiptNo, sku, productName, variant,
        quantity, unitPrice, branchCode, saleDate, payments
      ] = line.split(',');

      this.addRow({
        receiptNo: receiptNo?.trim(),
        sku: sku?.trim(),
        productName: productName?.trim(),
        variant: variant?.trim() || this.DEFAULT_VARIANT,
        quantity: Number(quantity),
        unitPrice: Number(unitPrice),
        branchCode: branchCode?.trim() || this.DEFAULT_BRANCH,
        saleDate: saleDate || new Date().toISOString(),
        payments: payments?.trim()
      });
    }
  }

  /* =========================
     EXCEL
     ========================= */

  async downloadExcelTemplate() {

    const workbook = new ExcelJS.Workbook();
    const worksheet = workbook.addWorksheet('Sales', {
      views: [{ state: 'frozen', ySplit: 1 }]
    });

    /* ============================
       DEFINE COLUMNS
       ============================ */
    worksheet.columns = [
      { header: 'receiptNo', key: 'receiptNo', width: 16 },
      { header: 'sku', key: 'sku', width: 16 },
      { header: 'productName', key: 'productName', width: 28 },
      { header: 'variant', key: 'variant', width: 14 },
      { header: 'quantity', key: 'quantity', width: 10 },
      { header: 'unitPrice', key: 'unitPrice', width: 14 },
      { header: 'branchCode', key: 'branchCode', width: 14 },
      { header: 'saleDate', key: 'saleDate', width: 18 },
      { header: 'payments', key: 'payments', width: 32 }
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
    const example = worksheet.addRow({
      receiptNo: 'R-001',
      sku: 'SKU-001',
      productName: '',
      variant: 'STANDARD',
      quantity: 2,
      unitPrice: 4500,
      branchCode: 'MAIN',
      saleDate: new Date(),
      payments: 'CASH|9000'
    });

    example.eachCell(cell => {
      cell.alignment = { vertical: 'middle', horizontal: 'left' };
      cell.border = {
        top: { style: 'thin' },
        left: { style: 'thin' },
        bottom: { style: 'thin' },
        right: { style: 'thin' }
      };
    });

    /* 🔥 DATE TYPE */
    example.getCell('saleDate').numFmt = 'dd/mm/yyyy';

    /* ============================
       EMPTY DATA ROWS
       ============================ */
    const ROW_COUNT = 200;

    for (let i = 0; i < ROW_COUNT; i++) {
      const row = worksheet.addRow({
        receiptNo: '',
        sku: '',
        productName: '',
        variant: 'STANDARD',
        quantity: 1,
        unitPrice: '',
        branchCode: 'MAIN',
        saleDate: '',
        payments: ''
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

      /* Ensure date column stays date-typed */
      row.getCell('saleDate').numFmt = 'dd/mm/yyyy';
    }

    /* ============================
       FINALIZE
       ============================ */
    const buffer = await workbook.xlsx.writeBuffer();

    saveAs(
      new Blob([buffer], {
        type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
      }),
      'sales-bulk-template.xlsx'
    );
  }

  importExcel(event: Event) {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;

    const reader = new FileReader();
    reader.onload = e => {
      const wb = XLSX.read(e.target?.result, { type: 'binary' });
      const sheet = wb.Sheets[wb.SheetNames[0]];
      const rows = XLSX.utils.sheet_to_json<any>(sheet, { defval: '' });

      this.rows.clear();
      rows.forEach(r => {
        this.addRow({
          ...r,
          saleDate: this.parseExcelDate(r.saleDate)
        });
      });
    };

    reader.readAsBinaryString(input.files[0]);
  }

  private parseExcelDate(value: any): Date | null {
    if (!value) return null;

    // Case 1: already a Date (ExcelJS, datepicker, etc.)
    if (value instanceof Date) {
      return value;
    }

    // Case 2: Excel serial number
    if (typeof value === 'number') {
      // Excel epoch = 1899-12-30
      const utcDays = value - 25569;
      return new Date(utcDays * 86400 * 1000);
    }

    // Case 3: ISO / string date
    if (typeof value === 'string') {
      const parsed = new Date(value);
      if (!isNaN(parsed.getTime())) {
        return parsed;
      }
    }

    return null;
  }

  /* =========================
     SUBMIT (IDENTICAL FLOW TO USERS)
     ========================= */

  submit() {
    if (this.form.invalid) return;
    this.submitting = true;

    const payload: BulkRequest<any> = {
      items: this.rows.controls.map(r => ({
        receiptNo: r.value.receiptNo,
        sku: r.value.sku || null,
        productName: r.value.productName || null,
        variant: r.value.variant,
        quantity: r.value.quantity,
        unitPrice: r.value.unitPrice,
        branchCode: r.value.branchCode,
        saleDate: r.value.saleDate
          ? new Date(r.value.saleDate).toISOString()
          : null,
        payments: r.value.payments || null
      })),
      options: {
        dryRun: this.form.value.dryRun
      }
    };

    this.salesService.import(this.form.value.mode, payload).subscribe({
      next: (res: BulkResult<any>) => {

        this.rows.controls.forEach(r =>
          r.patchValue({ _error: '' }, { emitEvent: false })
        );

        res.errors?.forEach(e => {
          this.rows.at(e.row - 1)?.patchValue({ _error: e.message });
        });

        if (!this.form.value.dryRun && res.failed === 0) {
          this.snackbar.open(
            `${res.success} sales imported successfully`,
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
            title: 'Sales Import Preview',
            dryRun: this.form.value.dryRun,
            result: res,
            confirmLabel: 'Import Now',
            columns: [
              { key: 'receiptNo', label: 'Receipt' },
              { key: 'productName', label: 'Product' },
              { key: 'sku', label: 'SKU' },
              { key: 'quantity', label: 'Qty' },
              { key: 'unitPrice', label: 'Price' },
              { key: 'branchCode', label: 'Branch' }
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
          'Sales bulk import failed',
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