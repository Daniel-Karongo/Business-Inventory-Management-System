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

import { CustomerService } from '../../services/customer.service';
import { BulkRequest, BulkResult } from '../../../../shared/models/bulk-import.model';
import { BulkImportResultDialogComponent } from '../../../../shared/components/bulk-import-result-dialog/bulk-import-result-dialog.component';
import { CustomerResponse } from '../../models/customer.model';

@Component({
  standalone: true,
  selector: 'app-customer-bulk-import-dialog',
  templateUrl: './customer-bulk-import-dialog.component.html',
  styleUrls: ['./customer-bulk-import-dialog.component.scss'],
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
    MatSnackBarModule
  ]
})
export class CustomerBulkImportDialogComponent implements OnInit {

  form!: FormGroup;
  submitting = false;

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<CustomerBulkImportDialogComponent>,
    private customerService: CustomerService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog
  ) {}

  ngOnInit(): void {
    this.form = this.fb.group({
      dryRun: [true],
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
      name: [data?.name ?? '', Validators.required],
      phones: [data?.phones ?? ''],
      emails: [data?.emails ?? ''],
      type: [data?.type ?? 'INDIVIDUAL', Validators.required],
      gender: [data?.gender ?? ''],
      address: [data?.address ?? ''],
      notes: [data?.notes ?? ''],
      _error: ['']
    });
  }

  addRow(data?: any) {
    this.rows.push(this.createRow(data));
  }

  removeRow(i: number) {
    this.rows.removeAt(i);
  }

  /* =========================
     CSV
     ========================= */

  downloadCsvTemplate() {
    const csv =
`name,phones,emails,type,gender,address,notes
John Doe,0712345678,john@doe.com,INDIVIDUAL,MALE,Nairobi,VIP customer`;

    saveAs(
      new Blob([csv], { type: 'text/csv' }),
      'customers-bulk-template.csv'
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
    const lines = text.split('\n').slice(1);
    this.rows.clear();

    for (const line of lines) {
      if (!line.trim()) continue;

      const [
        name,
        phones,
        emails,
        type,
        gender,
        address,
        notes
      ] = line.split(',');

      // 🔥 meaningful row check
      if (!name?.trim() && !phones?.trim() && !emails?.trim()) continue;

      this.addRow({
        name: name?.trim(),
        phones: phones?.trim(),
        emails: emails?.trim(),
        type: type?.trim() || 'INDIVIDUAL',
        gender: gender?.trim(),
        address: address?.trim(),
        notes: notes?.trim()
      });
    }
  }

  /* =========================
     EXCEL
     ========================= */

  async downloadExcelTemplate() {
    const workbook = new ExcelJS.Workbook();
    const worksheet = workbook.addWorksheet('Customers', {
      views: [{ state: 'frozen', ySplit: 1 }]
    });

    worksheet.columns = [
      { header: 'name', key: 'name', width: 22 },
      { header: 'phones', key: 'phones', width: 22 },
      { header: 'emails', key: 'emails', width: 32 },
      { header: 'type', key: 'type', width: 14 },
      { header: 'gender', key: 'gender', width: 14 },
      { header: 'address', key: 'address', width: 26 },
      { header: 'notes', key: 'notes', width: 32 }
    ];

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

    const exampleRow = worksheet.addRow({
      name: 'John Doe',
      phones: '0712345678',
      emails: 'john@doe.com',
      type: 'INDIVIDUAL',
      gender: 'MALE',
      address: 'Nairobi',
      notes: 'VIP customer'
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

    exampleRow.getCell('phones').numFmt = '@';

    const ROW_COUNT = 200;
    for (let i = 0; i < ROW_COUNT; i++) {
      const row = worksheet.addRow({
        name: '',
        phones: '',
        emails: '',
        type: 'INDIVIDUAL',
        gender: '',
        address: '',
        notes: ''
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

      row.getCell('phones').numFmt = '@';
    }

    const buffer = await workbook.xlsx.writeBuffer();
    saveAs(
      new Blob([buffer], {
        type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
      }),
      'customers-bulk-template.xlsx'
    );
  }

  importExcel(event: Event) {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;

    const reader = new FileReader();

    reader.onload = e => {
      const wb = XLSX.read(e.target?.result, { type: 'binary' });
      const sheet = wb.Sheets[wb.SheetNames[0]];

      const rawRows = XLSX.utils.sheet_to_json<any>(sheet, { defval: '' });
      this.rows.clear();

      rawRows
        .filter(r =>
          String(r.name || '').trim() ||
          String(r.phones || '').trim() ||
          String(r.emails || '').trim()
        )
        .forEach(r => {
          this.addRow({
            name: String(r.name || '').trim(),
            phones: String(r.phones || '').trim(),
            emails: String(r.emails || '').trim(),
            type: String(r.type || 'INDIVIDUAL').trim(),
            gender: String(r.gender || '').trim(),
            address: String(r.address || '').trim(),
            notes: String(r.notes || '').trim()
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
        phone: r.value.phones,
        email: r.value.emails,
        type: r.value.type,
        gender: r.value.gender || null,
        address: r.value.address,
        notes: r.value.notes
      })),
      options: {
        dryRun: this.form.value.dryRun,
        skipDuplicates: true
      }
    };

    this.customerService.bulkImport(payload).subscribe({
      next: (res: BulkResult<CustomerResponse>) => {

        this.rows.controls.forEach(r =>
          r.patchValue({ _error: '' }, { emitEvent: false })
        );

        res.errors?.forEach(e => {
          this.rows.at(e.row - 1)?.patchValue({ _error: e.message });
        });

        if (!this.form.value.dryRun && res.failed === 0) {
          this.snackbar.open(
            `${res.success} customers imported successfully`,
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
            title: 'Customer Import Preview',
            dryRun: this.form.value.dryRun,
            result: res,
            confirmLabel: 'Import Now',
            columns: [
              { key: 'name', label: 'Name' },
              { key: 'phoneNumbers', label: 'Phones' },
              { key: 'email', label: 'Emails' },
              { key: 'type', label: 'Type' }
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
          'Customer bulk import failed',
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