import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormArray, FormBuilder, FormGroup, Validators, ReactiveFormsModule } from '@angular/forms';
import { MatDialog, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatSelectModule } from '@angular/material/select';
import { MatFormFieldModule } from '@angular/material/form-field';

import { InventoryService } from '../../services/inventory.service';
import { SupplierService } from '../../../suppliers/services/supplier.service';
import { BranchService } from '../../../branches/services/branch.service';
import * as XLSX from 'xlsx';
import { ProductSelectorDialogComponent } from '../../../sales/dialogs/product-selector-dialog/product-selector-dialog.component';
import { Product } from '../../../products/parent/models/product.model';
import { ProductVariant } from '../../../products/variant/models/product-variant.model';
import { ProductService } from '../../../products/parent/services/product.service';

@Component({
  selector: 'app-bulk-receive-stock-dialog',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatDialogModule,
    MatButtonModule,
    MatInputModule,
    MatIconModule,
    MatSelectModule,
    MatFormFieldModule
  ],
  templateUrl: './bulk-receive-stock-dialog.component.html',
  styleUrls: ['./bulk-receive-stock-dialog.component.scss']
})
export class BulkReceiveStockDialogComponent implements OnInit {

  loading = false;
  submitting = false;

  branches: any[] = [];
  suppliers: any[] = [];

  form!: FormGroup;

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<BulkReceiveStockDialogComponent>,
    private inventoryService: InventoryService,
    private supplierService: SupplierService,
    private branchService: BranchService,
    private productService: ProductService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {
    this.form = this.fb.group({
      branchId: ['', Validators.required],
      reference: ['', Validators.required],
      note: [''],
      rows: this.fb.array([])
    });

    this.addRow(); // start with one row
    this.loadBranches();
    this.loadSuppliers();
  }

  /* =========================
     LOADERS
     ========================= */

  private loadBranches() {
    this.branchService.getAll(false).subscribe(b =>
      this.branches = b.filter(x => !!x.id)
    );
  }

  private loadSuppliers() {
    this.supplierService.getAll(false).subscribe(s =>
      this.suppliers = s.filter(x => !!x.id)
    );
  }

  variantOptions = new Map<number, ProductVariant[]>();

  loadVariantsForRow(rowIndex: number, productId: string) {
    this.productService.getVariants(productId).subscribe(variants => {
      this.variantOptions.set(rowIndex, variants);

      // 🔥 Auto-select if only one variant exists
      if (variants.length === 1) {
        const row = this.rows.at(rowIndex);
        const ctrl = row?.get('productVariantId');

        if (ctrl && !ctrl.value) {
          ctrl.setValue(variants[0].id);
        }
      }
    });
  }

  /* =========================
     ROWS
     ========================= */

  get rows(): FormArray {
    return this.form.get('rows') as FormArray;
  }

  addRow(product?: Product, supplierId?: string) {
    const resolvedSupplier =
      supplierId ??
      this.getDefaultSupplier() ??
      '';

    const row = this.fb.group({
      productId: [product?.id || '', Validators.required],
      productName: [product?.name || ''],

      variantMode: ['EXISTING', Validators.required],
      productVariantId: [''],
      classification: [''],

      supplierId: [resolvedSupplier, Validators.required],

      unitsSupplied: [1, [Validators.required, Validators.min(1)]],
      unitCost: [0, [Validators.required, Validators.min(0.01)]],
      sellingPrice: [null],

      _error: ['']
    });

    this.rows.push(row);

    // 🔥 IMPORTANT: wire propagation ONLY on first row
    if (this.rows.length === 1) {
      this.wireSupplierPropagation(row);
    }
  }

  removeRow(i: number) {
    this.rows.removeAt(i);
  }

  openProductSelector() {
    const ref = this.dialog.open(ProductSelectorDialogComponent, {
      width: '900px'
    });

    ref.afterClosed().subscribe((products: Product[]) => {
      if (!products?.length) return;

      // 🔥 Remove initial empty row if untouched
      if (
        this.rows.length === 1 &&
        !this.rows.at(0).value.productId
      ) {
        this.rows.clear();
      }

      const defaultSupplier = this.getDefaultSupplier();

      products.forEach(p => {
        this.addRow(p, defaultSupplier || undefined);
        this.loadVariantsForRow(this.rows.length - 1, p.id);
      });
    });
  }

  private getDefaultSupplier(): string | null {
    const first = this.rows.at(0);
    return first?.value?.supplierId || null;
  }

  private wireSupplierPropagation(row: FormGroup) {
    const sourceCtrl = row.get('supplierId');
    if (!sourceCtrl) return;

    sourceCtrl.valueChanges.subscribe(supplierId => {
      if (!supplierId) return;

      this.rows.controls.forEach(r => {
        if (r === row) return;

        const targetCtrl = r.get('supplierId');
        if (!targetCtrl) return;          // ✅ strict-safe
        if (targetCtrl.value) return;     // do not override user choice

        targetCtrl.setValue(supplierId, { emitEvent: false });
      });
    });
  }

  /* =========================
     CSV IMPORT
     ========================= */

  downloadTemplate() {
    const csv =
      `productId,variantMode,classification,unitsSupplied,unitCost,supplierId,sellingPrice
UUID-OF-PRODUCT,NEW,Standard,10,2500,UUID-OF-SUPPLIER,3200`;

    const blob = new Blob([csv], { type: 'text/csv' });
    const url = URL.createObjectURL(blob);

    const a = document.createElement('a');
    a.href = url;
    a.download = 'bulk-receive-template.csv';
    a.click();
    URL.revokeObjectURL(url);
  }

  importCsv(event: any) {
    const file = event.target.files[0];
    if (!file) return;

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
        productId,
        variantMode,
        classification,
        unitsSupplied,
        unitCost,
        supplierId,
        sellingPrice
      ] = line.split(',');

      this.rows.push(this.fb.group({
        productId: productId?.trim(),
        variantMode: variantMode?.trim() || 'EXISTING',
        classification: classification?.trim(),
        productVariantId: '',
        newVariantSku: '',
        supplierId: supplierId?.trim(),
        unitsSupplied: +unitsSupplied,
        unitCost: +unitCost,
        sellingPrice: sellingPrice ? +sellingPrice : null,
        _error: ['']
      }));
    }
  }

  downloadExcelTemplate() {
    const sheetName = 'ReceiveStock';

    const rows = [
      {
        productId: 'UUID-OF-PRODUCT',
        variantMode: 'NEW',
        productVariantId: '',
        classification: 'Standard',
        newVariantSku: '',
        supplierId: 'UUID-OF-SUPPLIER',
        unitsSupplied: 10,
        unitCost: 2500,
        sellingPrice: 3200
      }
    ];

    const worksheet = XLSX.utils.json_to_sheet(rows, {
      header: [
        'productId',
        'variantMode',
        'productVariantId',
        'classification',
        'newVariantSku',
        'supplierId',
        'unitsSupplied',
        'unitCost',
        'sellingPrice'
      ]
    });

    const workbook: XLSX.WorkBook = {
      Sheets: { [sheetName]: worksheet },
      SheetNames: [sheetName]
    };

    XLSX.writeFile(workbook, 'bulk-receive-template.xlsx');
  }

  importExcel(event: Event) {
    const input = event.target as HTMLInputElement;
    if (!input.files || !input.files.length) return;

    const file = input.files[0];
    const reader = new FileReader();

    reader.onload = (e: any) => {
      const workbook = XLSX.read(e.target.result, { type: 'binary' });
      const sheetName = workbook.SheetNames[0];
      const sheet = workbook.Sheets[sheetName];

      const rows = XLSX.utils.sheet_to_json<any>(sheet, { defval: '' });

      this.rows.clear();

      rows.forEach((r, index) => {
        const fg = this.fb.group({
          productId: [r.productId, Validators.required],

          variantMode: [r.variantMode || 'EXISTING'],
          productVariantId: [r.productVariantId || ''],
          classification: [r.classification || ''],
          newVariantSku: [r.newVariantSku || ''],

          supplierId: [r.supplierId, Validators.required],
          unitsSupplied: [
            Number(r.unitsSupplied),
            [Validators.required, Validators.min(1)]
          ],
          unitCost: [
            Number(r.unitCost),
            [Validators.required, Validators.min(0.01)]
          ],

          sellingPrice: r.sellingPrice
            ? Number(r.sellingPrice)
            : null,

          _error: ['']
        });

        this.validateRow(fg, index);
        this.rows.push(fg);
      });
    };

    reader.readAsBinaryString(file);
  }

  private validateRow(row: FormGroup, index: number) {
    const mode = row.value.variantMode;

    if (mode === 'EXISTING' && !row.value.productVariantId) {
      row.patchValue({
        _error: 'productVariantId is required when variantMode=EXISTING'
      });
    }

    if (mode === 'NEW' && !row.value.classification) {
      row.patchValue({
        _error: 'classification is required when variantMode=NEW'
      });
    }

    if (row.invalid && !row.value._error) {
      row.patchValue({
        _error: 'Invalid values in row'
      });
    }
  }

  /* =========================
     SUBMIT
     ========================= */

  submit() {
    const invalidRows = this.rows.controls.filter(r => r.value._error);

    if (invalidRows.length) {
      this.snackbar.open(
        `${invalidRows.length} row(s) have errors. Fix before submitting.`,
        'Close',
        { duration: 4000 }
      );
      return;
    }

    if (this.form.invalid) {
      this.snackbar.open('Fix validation errors before submitting', 'Close', { duration: 3000 });
      return;
    }

    this.submitting = true;

    const payload = {
      items: this.rows.controls.map(r => ({
        productId: r.value.productId,
        productVariantId:
          r.value.variantMode === 'EXISTING' ? r.value.productVariantId : null,
        classification:
          r.value.variantMode === 'NEW' ? r.value.classification : null,
        newVariantSku: r.value.newVariantSku,
        branchId: this.form.value.branchId,
        sellingPrice: r.value.sellingPrice,
        reference: this.form.value.reference,
        note: this.form.value.note,
        suppliers: [{
          supplierId: r.value.supplierId,
          unitsSupplied: r.value.unitsSupplied,
          unitCost: r.value.unitCost
        }]
      }))
    };

    this.inventoryService.bulkReceiveStock(payload).subscribe({
      next: () => {
        this.snackbar.open('Bulk stock received successfully', 'Close', { duration: 3000 });
        this.dialogRef.close(true);
      },
      error: err => {
        this.submitting = false;

        // row-level error fallback
        this.rows.controls.forEach(r =>
          r.patchValue({ _error: 'Failed to process row' })
        );

        this.snackbar.open('Some rows failed. See errors inline.', 'Close', { duration: 4000 });
      }
    });
  }

  close() {
    this.dialogRef.close();
  }
}