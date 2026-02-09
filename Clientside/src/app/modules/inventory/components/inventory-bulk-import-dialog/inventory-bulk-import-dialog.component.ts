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

import * as XLSX from 'xlsx';
import * as ExcelJS from 'exceljs';
import { saveAs } from 'file-saver';

import { MatDialog, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatTooltipModule } from '@angular/material/tooltip';

import { InventoryService } from '../../services/inventory.service';
import { InventoryBulkPreviewResult } from '../../models/inventory-bulk.model';
import { BulkRequest, BulkResult } from '../../../../shared/models/bulk-import.model';
import { BulkImportResultDialogComponent } from '../../../../shared/components/bulk-import-result-dialog/bulk-import-result-dialog.component';

@Component({
  standalone: true,
  selector: 'app-inventory-bulk-import-dialog',
  templateUrl: './inventory-bulk-import-dialog.component.html',
  styleUrls: ['./inventory-bulk-import-dialog.component.scss'],
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
export class InventoryBulkImportDialogComponent
  implements OnInit, AfterViewInit, OnDestroy {

  form!: FormGroup;
  submitting = false;

  /* 🔴 NEW */
  private errorIndex = 0;
  private errorRows: number[] = [];

  /* 🔴 NEW */
  @ViewChild('scrollContainer') scrollContainer!: ElementRef;
  @ViewChild('bottomAnchor') bottomAnchor!: ElementRef;

  constructor(
    private fb: FormBuilder,
    private inventoryService: InventoryService,
    private dialog: MatDialog,
    private dialogRef: MatDialogRef<InventoryBulkImportDialogComponent>,
    private snackbar: MatSnackBar
  ) {}

  ngOnInit(): void {
    this.form = this.fb.group({
      dryRun: [true],
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
     FORM
     ========================= */

  get rows(): FormArray {
    return this.form.get('rows') as FormArray;
  }

  /* 🔴 NEW (DERIVED) */
  get rowCount(): number {
    return this.rows.length;
  }

  private createRow(data?: any): FormGroup {
    return this.fb.group({
      productName: [data?.productName ?? '', Validators.required],
      variantClassification: [data?.variantClassification ?? 'STANDARD'],
      branchCode: [data?.branchCode ?? '', Validators.required],
      supplierName: [data?.supplierName ?? '', Validators.required],
      unitsSupplied: [data?.unitsSupplied ?? 1, [Validators.required, Validators.min(1)]],
      unitCost: [data?.unitCost ?? 0, [Validators.required, Validators.min(0.01)]],
      sellingPrice: [data?.sellingPrice ?? null],
      reference: [data?.reference ?? ''],
      note: [data?.note ?? ''],
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
     CSV (UNCHANGED)
     ========================= */

  downloadCsvTemplate() {
    const csv =
`productName,variantClassification,branchCode,supplierName,unitsSupplied,unitCost,sellingPrice,reference,note
Milk 1L,STANDARD,MAIN,Local Supplier,10,2500,3200,GRN-001,Initial stock`;

    saveAs(new Blob([csv], { type: 'text/csv' }),
      'inventory-bulk-receive-template.csv');
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
        productName,
        variantClassification,
        branchCode,
        supplierName,
        unitsSupplied,
        unitCost,
        sellingPrice,
        reference,
        note
      ] = line.split(',');

      this.addRow({
        productName,
        variantClassification: variantClassification || 'STANDARD',
        branchCode,
        supplierName,
        unitsSupplied: Number(unitsSupplied),
        unitCost: Number(unitCost),
        sellingPrice: sellingPrice ? Number(sellingPrice) : null,
        reference,
        note
      });
    }

    /* 🔴 NEW */
    this.snackbar.open(
      `✅ ${this.rowCount} rows loaded from CSV`,
      'Close',
      { duration: 3000 }
    );
  }

  /* =========================
     EXCEL TEMPLATE (UNCHANGED)
     ========================= */

  async downloadExcelTemplate() {
    const workbook = new ExcelJS.Workbook();
    const worksheet = workbook.addWorksheet('Inventory Receive', {
      views: [{ state: 'frozen', ySplit: 1 }]
    });

    worksheet.columns = [
      { header: 'productName', key: 'productName', width: 30 },
      { header: 'variantClassification', key: 'variantClassification', width: 22 },
      { header: 'branchCode', key: 'branchCode', width: 18 },
      { header: 'supplierName', key: 'supplierName', width: 26 },
      { header: 'unitsSupplied', key: 'unitsSupplied', width: 16 },
      { header: 'unitCost', key: 'unitCost', width: 16 },
      { header: 'sellingPrice', key: 'sellingPrice', width: 16 },
      { header: 'reference', key: 'reference', width: 18 },
      { header: 'note', key: 'note', width: 28 }
    ];

    const applyBorders = (row: ExcelJS.Row) => {
      row.eachCell({ includeEmpty: true }, cell => {
        cell.alignment = { vertical: 'middle', horizontal: 'left' };
        cell.border = {
          top: { style: 'thin' },
          left: { style: 'thin' },
          bottom: { style: 'thin' },
          right: { style: 'thin' }
        };
      });
    };

    const headerRow = worksheet.getRow(1);
    headerRow.font = { bold: true };
    applyBorders(headerRow);

    const exampleRow = worksheet.addRow({
      productName: 'Milk 1L',
      variantClassification: 'STANDARD',
      branchCode: 'MAIN',
      supplierName: 'Local Supplier',
      unitsSupplied: 10,
      unitCost: 2500,
      sellingPrice: 3200,
      reference: 'GRN-001',
      note: 'Initial stock'
    });

    applyBorders(exampleRow);

    for (let i = 0; i < 200; i++) {
      const row = worksheet.addRow({
        productName: '',
        variantClassification: 'STANDARD',
        branchCode: '',
        supplierName: '',
        unitsSupplied: '',
        unitCost: '',
        sellingPrice: '',
        reference: '',
        note: ''
      });
      applyBorders(row);
    }

    const buffer = await workbook.xlsx.writeBuffer();

    saveAs(
      new Blob([buffer], {
        type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
      }),
      'inventory-bulk-receive-template.xlsx'
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
      const wb = XLSX.read(e.target?.result, { type: 'binary' });
      const sheet = wb.Sheets[wb.SheetNames[0]];
      const rows = XLSX.utils.sheet_to_json<any>(sheet, { defval: '' });

      this.rows.clear();

      rows.forEach(r =>
        this.addRow({
          productName: r.productName,
          variantClassification: r.variantClassification || 'STANDARD',
          branchCode: r.branchCode,
          supplierName: r.supplierName,
          unitsSupplied: Number(r.unitsSupplied),
          unitCost: Number(r.unitCost),
          sellingPrice: r.sellingPrice || null,
          reference: r.reference,
          note: r.note
        })
      );

      /* 🔴 NEW */
      this.snackbar.open(
        `✅ ${this.rowCount} rows loaded from Excel`,
        'Close',
        { duration: 3000 }
      );
    };

    reader.readAsBinaryString(input.files[0]);
  }

  /* =========================
     SUBMIT (UNCHANGED + ADDITIVE UX)
     ========================= */

  submit() {
    if (this.form.invalid || this.submitting) return;
    this.submitting = true;

    const payload: BulkRequest<any> = {
      items: this.rows.value.map((r: any) => ({
        productName: r.productName,
        classification: r.variantClassification,
        branchCode: r.branchCode,
        sellingPrice: r.sellingPrice,
        reference: r.reference,
        note: r.note,
        suppliers: [{
          supplierName: r.supplierName,
          unitsSupplied: r.unitsSupplied,
          unitCost: r.unitCost
        }]
      })),
      options: { dryRun: this.form.value.dryRun }
    };

    this.inventoryService.bulkImport(payload).subscribe({
      next: (res: BulkResult<InventoryBulkPreviewResult>) => {

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
            'Inventory received successfully',
            'Close',
            { duration: 3000 }
          );
          this.dialogRef.close(true);
          return;
        }

        const preview = res.data.find(d => d && d.rows);

        if (!preview) {
          this.snackbar.open('No preview data available', 'Close', { duration: 3000 });
          this.submitting = false;
          return;
        }

        this.dialog.open(BulkImportResultDialogComponent, {
          width: '1100px',
          maxWidth: '95vw',
          data: {
            title: 'Inventory Receive Preview',
            dryRun: true,
            result: { ...res, data: preview.rows },
            confirmLabel: 'Confirm Receive',
            columns: [
              { key: 'productName', label: 'Product' },
              { key: 'variantClassification', label: 'Variant' },
              { key: 'branchName', label: 'Branch' },
              { key: 'unitsReceived', label: 'Units' },
              { key: 'unitCost', label: 'Unit Cost' },
              { key: 'totalCost', label: 'Total Cost' }
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
          'Inventory bulk import failed',
          'Close',
          { duration: 3000 }
        );
      }
    });
  }

  /* 🔴 NEW */
  private cacheErrors(res: BulkResult<any>) {
    this.errorRows = res.errors?.map(e => e.row) || [];
    this.errorIndex = 0;
  }

  /* 🔴 NEW */
  goToNextError() {
    if (!this.errorRows.length) return;
    this.goToLine(this.errorRows[this.errorIndex]);
    this.errorIndex = (this.errorIndex + 1) % this.errorRows.length;
  }

  /* 🔴 NEW */
  scrollToTop() {
    this.scrollContainer?.nativeElement.scrollTo({ top: 0, behavior: 'smooth' });
  }

  /* 🔴 NEW */
  scrollToBottom() {
    this.bottomAnchor?.nativeElement.scrollIntoView({ behavior: 'smooth' });
  }

  /* 🔴 NEW */
  goToLine(line: number) {
    if (!line || line < 1 || line > this.rows.length) return;

    const cards = this.scrollContainer.nativeElement.querySelectorAll('.row-card');
    const target = cards[line - 1] as HTMLElement;
    if (!target) return;

    target.scrollIntoView({ behavior: 'smooth', block: 'start' });
    target.classList.add('highlight-line');
    setTimeout(() => target.classList.remove('highlight-line'), 1500);
  }

  close() {
    this.dialogRef.close();
  }
}