import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReactiveFormsModule } from '@angular/forms';
import { MatDialogRef } from '@angular/material/dialog';

import { ProductService } from '../../services/product.service';
import { CategoryService } from '../../../../categories/services/category.service';
import { SupplierService } from '../../../../suppliers/services/supplier.service';

import { PRODUCT_BULK_IMPORT_CONFIG } from './product-bulk-import.config';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { BulkImportShellComponent } from '../../../../../shared/bulk-import/shell/bulk-import-shell.component';
import { BulkImportFormComponent } from '../../../../../shared/bulk-import/base/bulk-import-form.component';
import { BulkImportSubmitEngineService } from '../../../../../shared/bulk-import/engine/bulk-import-submit-engine.service';

@Component({
  standalone: true,
  selector: 'app-product-bulk-import-dialog',
  templateUrl: './product-bulk-import-dialog.component.html',
  styleUrls: ['./product-bulk-import-dialog.component.scss'],
  imports: [
    CommonModule,
    ReactiveFormsModule,
    BulkImportShellComponent,

    MatFormFieldModule,
    MatInputModule,
    MatCheckboxModule,
    MatButtonModule,
    MatIconModule
  ]
})
export class ProductBulkImportDialogComponent
  extends BulkImportFormComponent<any, any, any>
  implements OnInit {

  config = PRODUCT_BULK_IMPORT_CONFIG;

  categories: any[] = [];
  suppliers: any[] = [];

  constructor(
    private productService: ProductService,
    private categoryService: CategoryService,
    private supplierService: SupplierService,
    private submitEngine: BulkImportSubmitEngineService,
    private dialogRef: MatDialogRef<ProductBulkImportDialogComponent>
  ) {
    super();
  }

  ngOnInit() {
    this.initForm();
    this.loadMeta();
  }

  private loadMeta() {
    Promise.all([
      this.categoryService.getAll('flat', false).toPromise(),
      this.supplierService.getAll(false).toPromise()
    ]).then(([c, s]) => {
      this.categories = c || [];
      this.suppliers = s || [];
    });
  }

  submit() {
    if (this.form.invalid) return;

    const payload = {
      items: this.rows.controls.map(r =>
        this.config.mapRowToItem(r.value)
      ),
      options: {
        dryRun: this.form.value.dryRun,
        skipDuplicates: true
      }
    };

    this.submitEngine.execute({
      submitFn: req => this.productService.bulkImport(req),
      payload,
      rows: this.rows.controls,
      dryRun: this.form.value.dryRun,
      title: this.config.title,
      confirmLabel: this.config.confirmLabel,
      columns: this.config.previewColumns,
      onErrorsApplied: res => {
        this.cacheErrors(res);

        if (this.config.mapPreviewRow && Array.isArray(res.data)) {
          res.data = res.data.map(r => this.config.mapPreviewRow!(r));
        }
      },
      onFinalSuccess: () => this.dialogRef.close(true),
      onConfirmRetry: () => {
        this.form.patchValue({ dryRun: false });
        this.submit();
      }
    });
  }

  async importExcel(file: File | undefined) {
    if (!file) return;
    const mode = await this.confirmMerge();
    if (!mode) return;
    super.importExcelFile(file, undefined, mode);
  }

  async importCsv(file: File | undefined) {
    if (!file) return;
    const mode = await this.confirmMerge();
    if (!mode) return;
    super.importCsvFile(file, mode);
  }

  close() {
    this.dialogRef.close();
  }
}
