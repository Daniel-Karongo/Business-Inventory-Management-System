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
import { saveAs } from 'file-saver';

import { MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatFormFieldModule } from '@angular/material/form-field';

import * as XLSX from 'xlsx';

import { BranchService } from '../../services/branch.service';
import { BranchBulkRow } from '../../models/branch.model';
import { BranchDTO } from '../../models/branch.model';
import { BulkRequest, BulkResult } from '../../../../shared/models/bulk-import.model';
import { MatDialog } from '@angular/material/dialog';
import { BulkImportResultDialogComponent } from '../../../../shared/components/bulk-import-result-dialog/bulk-import-result-dialog.component';

@Component({
  selector: 'app-branch-bulk-import-dialog',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatDialogModule,
    MatButtonModule,
    MatInputModule,
    MatIconModule,
    MatCheckboxModule,
    MatFormFieldModule
  ],
  templateUrl: './branch-bulk-import-dialog.component.html',
  styleUrls: ['./branch-bulk-import-dialog.component.scss']
})
export class BranchBulkImportDialogComponent implements OnInit {

  loading = false;
  submitting = false;

  form!: FormGroup;

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<BranchBulkImportDialogComponent>,
    private branchService: BranchService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {
    this.form = this.fb.group({
      dryRun: [false],
      rows: this.fb.array([])
    });

    this.addRow(); // start with one row
  }

  /* =========================
     ROWS
     ========================= */

  get rows(): FormArray {
    return this.form.get('rows') as FormArray;
  }

  private createRow(data?: Partial<BranchBulkRow>) {
    const fg = this.fb.group({
      branchCode: [data?.branchCode ?? '', Validators.required],
      name: [data?.name ?? '', Validators.required],
      location: [data?.location ?? ''],
      phone: [data?.phone ?? ''],
      email: [data?.email ?? ''],
      _error: ['']
    });

    // 🔤 AUTO-UPPERCASE
    fg.get('branchCode')!.valueChanges.subscribe(v => {
      if (!v) return;
      const upper = v.toUpperCase();
      if (v !== upper) {
        fg.get('branchCode')!.setValue(upper, { emitEvent: false });
      }
    });

    return fg;
  }

  addRow(data?: Partial<BranchBulkRow>) {
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
      `branchCode,name,location,phone,email
MAIN,Main Branch,Nairobi,0700000000,main@example.com`;

    const blob = new Blob([csv], { type: 'text/csv' });
    const url = URL.createObjectURL(blob);

    const a = document.createElement('a');
    a.href = url;
    a.download = 'branches-bulk-template.csv';
    a.click();

    URL.revokeObjectURL(url);
  }

  importCsv(event: Event) {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;

    const file = input.files[0];
    const reader = new FileReader();

    reader.onload = () => {
      const text = reader.result as string;
      this.parseCsv(text);
    };

    reader.readAsText(file);
  }

  private parseCsv(text: string) {
    const lines = text.split('\n').slice(1);
    this.rows.clear();

    for (const line of lines) {
      if (!line.trim()) continue;

      const [
        branchCode,
        name,
        location,
        phone,
        email
      ] = line.split(',');

      this.addRow({
        branchCode: branchCode?.trim(),
        name: name?.trim(),
        location: location?.trim(),
        phone: phone?.trim(),
        email: email?.trim()
      });
    }
  }

  /* =========================
     EXCEL
     ========================= */

  async downloadExcelTemplate() {

    const workbook = new ExcelJS.Workbook();
    const worksheet = workbook.addWorksheet('Branches', {
      views: [{ state: 'frozen', ySplit: 1 }]
    });

    /* ============================
       DEFINE COLUMNS
       ============================ */
    worksheet.columns = [
      { header: 'branchCode', key: 'branchCode', width: 14 },
      { header: 'name', key: 'name', width: 22 },
      { header: 'location', key: 'location', width: 22 },
      { header: 'phone', key: 'phone', width: 16 },
      { header: 'email', key: 'email', width: 28 }
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
       PLACEHOLDER / EXAMPLE ROW
       ============================ */
    const exampleRow = worksheet.addRow({
      branchCode: 'BR001',
      name: 'Main Branch',
      location: 'Nairobi',
      phone: '0712345678',
      email: 'main@example.com'
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

    // 🔥 Force phone column to TEXT
    exampleRow.getCell('phone').numFmt = '@';

    /* ============================
       EMPTY DATA ROWS
       ============================ */
    const ROW_COUNT = 200;

    for (let i = 0; i < ROW_COUNT; i++) {
      const row = worksheet.addRow({
        branchCode: '',
        name: '',
        location: '',
        phone: '',
        email: ''
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

      row.getCell('phone').numFmt = '@';
    }

    /* ============================
       FINALIZE
       ============================ */
    const buffer = await workbook.xlsx.writeBuffer();

    saveAs(
      new Blob([buffer], {
        type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
      }),
      'branch-import-template.xlsx'
    );
  }

  importExcel(event: Event) {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;

    const file = input.files[0];
    const reader = new FileReader();

    reader.onload = (e: any) => {
      const workbook = XLSX.read(e.target.result, { type: 'binary' });
      const sheet = workbook.Sheets[workbook.SheetNames[0]];
      const rawRows = XLSX.utils.sheet_to_json<any>(sheet, { defval: '' });

      this.rows.clear();

      rawRows
        // 🔥 FILTER OUT EMPTY ROWS
        .filter(r =>
          (r.branchCode && String(r.branchCode).trim()) ||
          (r.name && String(r.name).trim())
        )
        .forEach(r => {
          this.addRow({
            branchCode: String(r.branchCode || '').trim(),
            name: String(r.name || '').trim(),
            location: String(r.location || '').trim(),
            phone: String(r.phone || '').trim(),
            email: String(r.email || '').trim()
          });
        });

      if (this.rows.length === 0) {
        this.snackbar.open(
          'No valid rows found in the file',
          'Close',
          { duration: 3000 }
        );
      }
    };

    reader.readAsBinaryString(file);
  }

  /* =========================
     SUBMIT
     ========================= */

  submit() {
    if (this.form.invalid) {
      this.snackbar.open(
        'Fix validation errors before submitting',
        'Close',
        { duration: 3000 }
      );
      return;
    }

    this.submitting = true;

    const payload: BulkRequest<BranchBulkRow> = {
      items: this.rows.controls.map(r => ({
        branchCode: r.value.branchCode,
        name: r.value.name,
        location: r.value.location,
        phone: r.value.phone,
        email: r.value.email
      })),
      options: {
        dryRun: this.form.value.dryRun,
        skipDuplicates: true
      }
    };

    this.branchService.bulkImport(payload).subscribe({
      next: (res: BulkResult<BranchDTO>) => {

        // clear previous errors
        this.rows.controls.forEach(r =>
          r.patchValue({ _error: '' }, { emitEvent: false })
        );

        res.errors?.forEach(e => {
          const row = this.rows.at(e.row - 1);
          row?.patchValue({ _error: e.message });
        });

        // ACTUAL IMPORT SUCCESS → CLOSE EVERYTHING
        if (!this.form.value.dryRun && res.failed === 0) {
          this.snackbar.open(
            `${res.success} branches imported successfully`,
            'Close',
            { duration: 3000 }
          );

          // 🔥 CLOSE THIS DIALOG AND SIGNAL SUCCESS
          this.dialogRef.close(true);
          return;
        }

        // DRY RUN OR PARTIAL FAIL → SHOW RESULT DIALOG
        this.dialog.open(BulkImportResultDialogComponent, {
          width: '1100px',
          maxWidth: '95vw',
          data: {
            title: 'Branch Import Preview',
            dryRun: this.form.value.dryRun,
            result: res,
            confirmLabel: 'Import Now',
            columns: [
              { key: 'branchCode', label: 'Code' },
              { key: 'name', label: 'Name' },
              { key: 'location', label: 'Location' },
              { key: 'phone', label: 'Phone' },
              { key: 'email', label: 'Email' }
            ]
          }
        }).afterClosed().subscribe(confirm => {
          if (confirm) {
            this.form.patchValue({ dryRun: false });
            this.submit(); // 🔁 re-submit as actual import
          }
        });

        this.submitting = false;
      },
      error: () => {
        this.submitting = false;
        this.snackbar.open(
          'Bulk import failed. Try again.',
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