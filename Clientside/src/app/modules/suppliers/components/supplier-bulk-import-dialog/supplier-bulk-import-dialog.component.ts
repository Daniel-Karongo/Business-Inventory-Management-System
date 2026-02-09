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
import * as XLSX from 'xlsx';
import { saveAs } from 'file-saver';

import { MatDialog, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatTooltipModule } from '@angular/material/tooltip';

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
    MatSnackBarModule,
    MatTooltipModule
  ]
})
export class SupplierBulkImportDialogComponent
  implements OnInit, AfterViewInit, OnDestroy {

  form!: FormGroup;
  submitting = false;
  categories: any[] = [];

  /* 🔴 UX ADDITIONS */
  private errorRows: number[] = [];
  private errorIndex = 0;

  @ViewChild('scrollContainer') scrollContainer!: ElementRef;
  @ViewChild('bottomAnchor') bottomAnchor!: ElementRef;

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<SupplierBulkImportDialogComponent>,
    private supplierService: SupplierService,
    private categoryService: CategoryService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog
  ) {}

  ngOnInit(): void {
    this.form = this.fb.group({
      dryRun: [true],
      rows: this.fb.array([])
    });

    this.loadCategories();
    this.addRow();
  }

  ngAfterViewInit() {
    window.addEventListener('keydown', this.handleKeyNav);
  }

  ngOnDestroy() {
    window.removeEventListener('keydown', this.handleKeyNav);
  }

  /* 🔴 KEYBOARD NAV */
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
     FORM
     ========================= */

  get rows(): FormArray {
    return this.form.get('rows') as FormArray;
  }

  /* 🔴 DERIVED */
  get rowCount(): number {
    return this.rows.length;
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
    this.categoryService
      .getAll('flat', false)
      .subscribe(c => this.categories = c || []);
  }

  /* =========================
     CSV (UNCHANGED)
     ========================= */

  downloadCsvTemplate() {
    const csv =
`name,emails,phones,address,region,categories
Acme Ltd,info@acme.com,0712345678,Nairobi,Nairobi,Electronics,Accessories`;

    saveAs(
      new Blob([csv], { type: 'text/csv' }),
      'suppliers-bulk-template.csv'
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

      const [name, emails, phones, address, region, categories] =
        line.split(',');

      this.addRow({
        name: name?.trim(),
        emails: emails?.trim(),
        phones: phones?.trim(),
        address: address?.trim(),
        region: region?.trim(),
        categories: categories?.trim()
      });
    }

    /* 🔴 UX */
    this.snackbar.open(
      `✅ ${this.rowCount} suppliers loaded from CSV`,
      'Close',
      { duration: 3000 }
    );
  }

  /* =========================
     EXCEL TEMPLATE (UNCHANGED)
     ========================= */

  async downloadExcelTemplate() {
    const workbook = new ExcelJS.Workbook();
    const worksheet = workbook.addWorksheet('Suppliers', {
      views: [{ state: 'frozen', ySplit: 1 }]
    });

    worksheet.columns = [
      { header: 'name', key: 'name', width: 26 },
      { header: 'emails', key: 'emails', width: 32 },
      { header: 'phones', key: 'phones', width: 22 },
      { header: 'address', key: 'address', width: 26 },
      { header: 'region', key: 'region', width: 18 },
      { header: 'categories', key: 'categories', width: 32 }
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

    exampleRow.getCell('phones').numFmt = '@';

    for (let i = 0; i < 200; i++) {
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

      row.getCell('phones').numFmt = '@';
    }

    const buffer = await workbook.xlsx.writeBuffer();

    saveAs(
      new Blob([buffer], {
        type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
      }),
      'suppliers-bulk-template.xlsx'
    );
  }

  /* =========================
     EXCEL IMPORT (UNCHANGED)
     ========================= */

  importExcel(event: Event) {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;

    const reader = new FileReader();
    reader.onload = e => {
      const workbook = XLSX.read(e.target?.result, { type: 'binary' });
      const sheet = workbook.Sheets[workbook.SheetNames[0]];
      const rawRows = XLSX.utils.sheet_to_json<any>(sheet, { defval: '' });

      this.rows.clear();

      rawRows
        .filter(r =>
          String(r.name || '').trim() ||
          String(r.emails || '').trim() ||
          String(r.phones || '').trim()
        )
        .forEach(r =>
          this.addRow({
            name: String(r.name || '').trim(),
            emails: String(r.emails || '').trim(),
            phones: String(r.phones || '').trim(),
            address: String(r.address || '').trim(),
            region: String(r.region || '').trim(),
            categories: String(r.categories || '').trim()
          })
        );

      /* 🔴 UX */
      this.snackbar.open(
        `✅ ${this.rowCount} suppliers loaded from Excel`,
        'Close',
        { duration: 3000 }
      );
    };

    reader.readAsBinaryString(input.files[0]);
  }

  /* =========================
     SUBMIT (UNCHANGED + UX)
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

        /* 🔴 UX */
        this.errorRows = res.errors?.map(e => e.row) || [];
        this.errorIndex = 0;

        if (this.errorRows.length) {
          setTimeout(() => this.goToLine(this.errorRows[0]), 200);
        }

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

  /* 🔴 UX HELPERS */

  goToLine(line: number) {
    if (!line || line < 1 || line > this.rows.length) return;

    const cards =
      this.scrollContainer.nativeElement.querySelectorAll('.row-card');

    const target = cards[line - 1] as HTMLElement;
    if (!target) return;

    target.scrollIntoView({ behavior: 'smooth', block: 'start' });
    target.classList.add('highlight-line');
    setTimeout(() => target.classList.remove('highlight-line'), 1500);
  }

  goToNextError() {
    if (!this.errorRows.length) return;
    this.goToLine(this.errorRows[this.errorIndex]);
    this.errorIndex = (this.errorIndex + 1) % this.errorRows.length;
  }

  scrollToTop() {
    this.scrollContainer.nativeElement.scrollTo({
      top: 0,
      behavior: 'smooth'
    });
  }

  scrollToBottom() {
    this.bottomAnchor.nativeElement.scrollIntoView({
      behavior: 'smooth'
    });
  }

  close() {
    this.dialogRef.close();
  }
}