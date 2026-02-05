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

import { forkJoin } from 'rxjs';

import { ProductService } from '../../services/product.service';

import { Product } from '../../models/product.model';
import { CategoryService } from '../../../../categories/services/category.service';
import { SupplierService } from '../../../../suppliers/services/supplier.service';
import { BulkRequest, BulkResult } from '../../../../../shared/models/bulk-import.model';
import { BulkImportResultDialogComponent } from '../../../../../shared/components/bulk-import-result-dialog/bulk-import-result-dialog.component';

@Component({
  standalone: true,
  selector: 'app-product-bulk-import-dialog',
  templateUrl: './product-bulk-import-dialog.component.html',
  styleUrls: ['./product-bulk-import-dialog.component.scss'],
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
export class ProductBulkImportDialogComponent implements OnInit {

  form!: FormGroup;
  submitting = false;

  categories: any[] = [];
  suppliers: any[] = [];

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ProductBulkImportDialogComponent>,
    private productService: ProductService,
    private categoryService: CategoryService,
    private supplierService: SupplierService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {
    this.form = this.fb.group({
      dryRun: [true],
      rows: this.fb.array([])
    });

    this.loadMeta();
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
      description: [data?.description ?? ''],
      categoryName: [data?.categoryName ?? '', Validators.required],
      supplierNames: [data?.supplierNames ?? ''],
      variants: [data?.variants ?? 'STANDARD'],
      minimumPercentageProfit: [data?.minimumPercentageProfit ?? ''],
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
     META
     ========================= */

  private loadMeta() {
    forkJoin([
      this.categoryService.getAll('flat', false),
      this.supplierService.getAll(false)
    ]).subscribe(([c, s]) => {
      this.categories = c || [];
      this.suppliers = s || [];
    });
  }

  /* =========================
     CSV
     ========================= */

  downloadCsvTemplate() {
    const csv =
      `name,description,categoryName,supplierNames,variants,minimumPercentageProfit
Milk 1L,Fresh milk,Dairy,Local Supplier,STANDARD,15`;

    saveAs(
      new Blob([csv], { type: 'text/csv' }),
      'products-bulk-template.csv'
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
        name, description, categoryName,
        supplierNames, variants, minimumPercentageProfit
      ] = line.split(',');

      this.addRow({
        name,
        description,
        categoryName,
        supplierNames,
        variants: variants || 'STANDARD',
        minimumPercentageProfit
      });
    }
  }

  /* =========================
     EXCEL
     ========================= */

  async downloadExcelTemplate() {

    const workbook = new ExcelJS.Workbook();
    const worksheet = workbook.addWorksheet('Products', {
      views: [{ state: 'frozen', ySplit: 1 }]
    });

    /* =========================
       DEFINE COLUMNS
       ========================= */
    worksheet.columns = [
      { header: 'name', key: 'name', width: 26 },
      { header: 'description', key: 'description', width: 36 },
      { header: 'categoryName', key: 'categoryName', width: 20 },
      { header: 'supplierNames', key: 'supplierNames', width: 28 },
      { header: 'variants', key: 'variants', width: 18 },
      { header: 'minimumPercentageProfit', key: 'minimumPercentageProfit', width: 16 }
    ];

    /* =========================
       HEADER STYLING
       ========================= */
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

    /* =========================
       EXAMPLE ROW
       ========================= */
    const exampleRow = worksheet.addRow({
      name: 'Milk 1L',
      description: 'Fresh whole milk',
      categoryName: 'Dairy',
      supplierNames: 'Local Supplier',
      variants: 'STANDARD',
      minimumPercentageProfit: 15
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

    /* =========================
       EMPTY DATA ROWS
       ========================= */
    const ROW_COUNT = 200;

    for (let i = 0; i < ROW_COUNT; i++) {
      const row = worksheet.addRow({
        name: '',
        description: '',
        categoryName: '',
        supplierNames: '',
        variants: 'STANDARD',
        minimumPercentageProfit: ''
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
    }

    /* =========================
       FINALIZE
       ========================= */
    const buffer = await workbook.xlsx.writeBuffer();

    saveAs(
      new Blob([buffer], {
        type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
      }),
      'products-bulk-template.xlsx'
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
      rows.forEach(r =>
        this.addRow({
          name: r.name,
          description: r.description,
          categoryName: r.categoryName,
          supplierNames: r.supplierNames,
          variants: r.variants || 'STANDARD',
          minimumPercentageProfit: r.minimumPercentageProfit
        })
      );
    };
    reader.readAsBinaryString(input.files[0]);
  }

  /* =========================
     SUBMIT
     ========================= */

  submit() {
    if (this.form.invalid || this.submitting) return;

    this.submitting = true;

    const payload: BulkRequest<any> = {
      items: this.rows.value.map((r: any) => ({
        name: r.name,
        description: r.description,
        categoryName: r.categoryName,
        supplierNames: r.supplierNames
          ? r.supplierNames
            .split(',')
            .map((s: string) => s.trim())
            .filter((s: string) => s.length > 0)
          : [],
        variants: r.variants
          ? r.variants
            .split(',')
            .map((v: string) => v.trim())
            .filter((v: string) => v.length > 0)
          : ['STANDARD'],
        minimumPercentageProfit: r.minimumPercentageProfit
      })),
      options: {
        dryRun: this.form.value.dryRun,
        skipDuplicates: true
      }
    };

    this.productService.bulkImport(payload).subscribe({
      next: (res: BulkResult<Product>) => {

        /* =========================
           CLEAR PREVIOUS ROW ERRORS
           ========================= */
        this.rows.controls.forEach(r =>
          r.patchValue({ _error: '' }, { emitEvent: false })
        );

        res.errors?.forEach(e => {
          this.rows.at(e.row - 1)?.patchValue({ _error: e.message });
        });

        /* =========================
           FINAL IMPORT SUCCESS
           ========================= */
        if (!this.form.value.dryRun && res.failed === 0) {

          this.snackbar.open(
            `${res.success} products imported successfully`,
            'Close',
            { duration: 3000 }
          );

          // 🔥 CLOSE BULK IMPORT DIALOG AND SIGNAL RELOAD
          this.dialogRef.close(true);
          return;
        }

        /* =========================
           ADAPT RESULT FOR PREVIEW
           ========================= */
        const adaptedResult: BulkResult<any> = {
          ...res,
          data: res.data.map(p => ({
            ...p,
            variantNames: Array.isArray(p.variants)
              ? p.variants
                .map((v: any) => v.classification)
                .filter(Boolean)
                .join(', ')
              : ''
          }))
        };

        /* =========================
           OPEN PREVIEW DIALOG
           ========================= */
        this.dialog.open(BulkImportResultDialogComponent, {
          width: '1100px',
          maxWidth: '95vw',
          data: {
            title: 'Product Import Preview',
            dryRun: true,
            result: adaptedResult,
            confirmLabel: 'Import Products',
            columns: [
              { key: 'name', label: 'Name' },
              { key: 'categoryName', label: 'Category' },
              { key: 'variantNames', label: 'Variants' },
              { key: 'minimumPercentageProfit', label: 'Min % Profit' }
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
          'Product bulk import failed',
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