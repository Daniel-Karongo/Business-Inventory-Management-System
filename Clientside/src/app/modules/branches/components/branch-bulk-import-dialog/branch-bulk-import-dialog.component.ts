import {
  Component,
  OnInit,
  ViewChild,
  ElementRef,
  AfterViewInit,
  OnDestroy
} from '@angular/core';
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

import { MatDialogModule, MatDialogRef, MatDialog } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatTooltipModule } from '@angular/material/tooltip';

import * as XLSX from 'xlsx';

import { BranchService } from '../../services/branch.service';
import { BranchBulkRow, BranchDTO } from '../../models/branch.model';
import { BulkRequest, BulkResult } from '../../../../shared/models/bulk-import.model';
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
    MatFormFieldModule,
    MatTooltipModule
  ],
  templateUrl: './branch-bulk-import-dialog.component.html',
  styleUrls: ['./branch-bulk-import-dialog.component.scss']
})
export class BranchBulkImportDialogComponent
  implements OnInit, AfterViewInit, OnDestroy {

  loading = false;
  submitting = false;

  form!: FormGroup;

  /* 🔴 NEW */
  private errorIndex = 0;
  private errorRows: number[] = [];

  /* 🔴 NEW */
  @ViewChild('scrollContainer') scrollContainer!: ElementRef;
  @ViewChild('bottomAnchor') bottomAnchor!: ElementRef;

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<BranchBulkImportDialogComponent>,
    private branchService: BranchService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog
  ) {}

  ngOnInit(): void {
    this.form = this.fb.group({
      dryRun: [false],
      rows: this.fb.array([])
    });

    this.addRow();
  }

  /* 🔴 NEW */
  ngAfterViewInit() {
    window.addEventListener('keydown', this.handleKeyNav);
  }

  ngOnDestroy() {
    window.removeEventListener('keydown', this.handleKeyNav);
  }

  /* 🔴 NEW */
  handleKeyNav = (e: KeyboardEvent) => {
    if (e.ctrlKey && e.key.toLowerCase() === 'g') {
      e.preventDefault();
      document.querySelector<HTMLInputElement>('#goToLineInput')?.focus();
    }

    if (e.key === 'Enter') {
      const el = document.activeElement as HTMLInputElement;
      if (el?.id === 'goToLineInput') {
        this.goToLine(el.valueAsNumber);
      }
    }
  };

  /* =========================
     ROWS (UNCHANGED)
     ========================= */

  get rows(): FormArray {
    return this.form.get('rows') as FormArray;
  }

  /* 🔴 NEW (DERIVED ONLY) */
  get rowCount(): number {
    return this.rows.length;
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
     CSV (UNCHANGED)
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

    const reader = new FileReader();
    reader.onload = () => this.parseCsv(reader.result as string);
    reader.readAsText(input.files[0]);
  }

  private parseCsv(text: string) {
    const lines = text.split('\n').slice(1);
    this.rows.clear();

    for (const line of lines) {
      if (!line.trim()) continue;

      const [branchCode, name, location, phone, email] = line.split(',');

      this.addRow({
        branchCode: branchCode?.trim(),
        name: name?.trim(),
        location: location?.trim(),
        phone: phone?.trim(),
        email: email?.trim()
      });
    }

    /* 🔴 NEW */
    this.snackbar.open(
      `✅ ${this.rowCount} branches loaded from CSV`,
      'Close',
      { duration: 3000 }
    );
  }

  /* =========================
     EXCEL (UNCHANGED)
     ========================= */

  async downloadExcelTemplate() {
    const workbook = new ExcelJS.Workbook();
    const worksheet = workbook.addWorksheet('Branches', {
      views: [{ state: 'frozen', ySplit: 1 }]
    });

    worksheet.columns = [
      { header: 'branchCode', key: 'branchCode', width: 14 },
      { header: 'name', key: 'name', width: 22 },
      { header: 'location', key: 'location', width: 22 },
      { header: 'phone', key: 'phone', width: 16 },
      { header: 'email', key: 'email', width: 28 }
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

    exampleRow.getCell('phone').numFmt = '@';

    for (let i = 0; i < 200; i++) {
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

    const reader = new FileReader();
    reader.onload = e => {
      const wb = XLSX.read(e.target?.result, { type: 'binary' });
      const sheet = wb.Sheets[wb.SheetNames[0]];
      const rawRows = XLSX.utils.sheet_to_json<any>(sheet, { defval: '' });

      this.rows.clear();

      rawRows
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
      } else {
        /* 🔴 NEW */
        this.snackbar.open(
          `✅ ${this.rowCount} branches loaded from Excel`,
          'Close',
          { duration: 3000 }
        );
      }
    };

    reader.readAsBinaryString(input.files[0]);
  }

  /* 🔴 NEW: SCROLL + ERRORS */

  scrollToTop() {
    this.scrollContainer?.nativeElement.scrollTo({ top: 0, behavior: 'smooth' });
  }

  scrollToBottom() {
    this.bottomAnchor?.nativeElement.scrollIntoView({ behavior: 'smooth' });
  }

  goToLine(line: number) {
    if (!line || line < 1 || line > this.rows.length) return;

    const cards = this.scrollContainer.nativeElement.querySelectorAll('.row-card');
    const target = cards[line - 1] as HTMLElement;
    if (!target) return;

    target.scrollIntoView({ behavior: 'smooth', block: 'start' });
    target.classList.add('highlight-line');
    setTimeout(() => target.classList.remove('highlight-line'), 1500);
  }

  private cacheErrors(res: BulkResult<any>) {
    this.errorRows = res.errors?.map(e => e.row) || [];
    this.errorIndex = 0;
  }

  goToNextError() {
    if (!this.errorRows.length) return;
    this.goToLine(this.errorRows[this.errorIndex]);
    this.errorIndex = (this.errorIndex + 1) % this.errorRows.length;
  }

  /* =========================
     SUBMIT (UNCHANGED)
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

        this.rows.controls.forEach(r =>
          r.patchValue({ _error: '' }, { emitEvent: false })
        );

        res.errors?.forEach(e => {
          this.rows.at(e.row - 1)?.patchValue({ _error: e.message });
        });

        /* 🔴 NEW */
        if (res.errors?.length) {
          setTimeout(() => this.goToLine(res.errors[0].row), 200);
        }

        this.cacheErrors(res);

        if (!this.form.value.dryRun && res.failed === 0) {
          this.snackbar.open(
            `${res.success} branches imported successfully`,
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
            this.submit();
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