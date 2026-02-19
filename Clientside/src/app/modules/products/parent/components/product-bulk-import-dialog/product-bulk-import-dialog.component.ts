import { CommonModule } from '@angular/common';
import { Component, OnDestroy, OnInit } from '@angular/core';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatDialog, MatDialogRef } from '@angular/material/dialog';

import { ProductService } from '../../services/product.service';

import { PRODUCT_BULK_IMPORT_CONFIG } from './product-bulk-import.config';

import { MatButtonModule } from '@angular/material/button';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';

import { BulkImportFormComponent } from '../../../../../shared/bulk-import/base/bulk-import-form.component';
import { BulkImportSubmitEngineService } from '../../../../../shared/bulk-import/engine/bulk-import-submit-engine.service';
import { BulkImportShellComponent } from '../../../../../shared/bulk-import/shell/bulk-import-shell.component';

import { BulkFileAssignmentComponent } from '../../../../../shared/bulk-import/files/bulk-file-assignment.component';
import { BulkAssignedFile } from '../../../../../shared/bulk-import/files/bulk-file.model';
import { BulkImportWithFilesComponent } from '../../../../../shared/bulk-import/base/bulk-import-with-files.component';
import { BulkFileFactoryService } from '../../../../../shared/bulk-import/files/bulk-file-factory.service';
import { BulkFileLifecycleService } from '../../../../../shared/bulk-import/files/bulk-file-lifecycle.service';
import { BulkFileImportEngine } from '../../../../../shared/bulk-import/files/bulk-file-import.engine';
import { BulkImportResultDialogComponent } from '../../../../../shared/components/bulk-import-result-dialog/bulk-import-result-dialog.component';
import { BulkCameraCaptureService } from '../../../../../shared/bulk-import/camera/bulk-camera-capture.service';

@Component({
  standalone: true,
  selector: 'app-product-bulk-import-dialog',
  templateUrl: './product-bulk-import-dialog.component.html',
  styleUrls: ['./product-bulk-import-dialog.component.scss'],
  imports: [
    CommonModule,
    ReactiveFormsModule,
    FormsModule,
    BulkImportShellComponent,
    MatFormFieldModule,
    MatInputModule,
    MatCheckboxModule,
    MatButtonModule,
    MatIconModule,
    BulkFileAssignmentComponent
  ]
})
export class ProductBulkImportDialogComponent
  extends BulkImportWithFilesComponent<any, any, any>
  implements OnInit, OnDestroy {

  config = PRODUCT_BULK_IMPORT_CONFIG;

  zipLoading = false;
  createMissingCategories = false;
  createMissingSuppliers = false;

  constructor(
    private productService: ProductService,
    private submitEngine: BulkImportSubmitEngineService,
    private dialogRef: MatDialogRef<ProductBulkImportDialogComponent>,
    fileFactory: BulkFileFactoryService,
    fileLifecycle: BulkFileLifecycleService,
    zipEngine: BulkFileImportEngine,
    camera: BulkCameraCaptureService
  ) {
    super(fileFactory, fileLifecycle, zipEngine, camera);
  }

  ngOnInit() {
    this.initForm();
  }

  getRowVariants(index: number): string[] {

    const value =
      this.rows.at(index).get('variants')?.value ?? '';

    return value
      .split(',')
      .map((v: string) => v.trim().toUpperCase())
      .filter(Boolean);
  }

  uppercaseVariants(index: number) {

    const control = this.rows.at(index).get('variants');
    if (!control) return;

    const value = control.value ?? '';

    control.setValue(
      value.toUpperCase(),
      { emitEvent: false }
    );
  }

  trackByIndex(index: number) {
    return index;
  }

  /* =========================================================
   EXCEL / CSV IMPORT (Inherited Engine)
========================================================= */

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

  /* =========================================================
   DIALOG CLOSE
========================================================= */

  close() {
    this.dialogRef.close();
  }

  /* =========================================================
     ROW NAMES (For unmatched assignment component)
  ========================================================= */

  get rowNames(): string[] {
    return this.rows.value.map((r: any) => r.name ?? '');
  }

  /* =========================================================
     DRY RUN (JSON ONLY)
  ========================================================== */

  /* =========================================================
     DRY RUN
  ========================================================= */

  submit() {

    const payload = {

      products: this.rows.value.map((r: any) => ({
        ...r,
        supplierNames: r.supplierNames
          ? r.supplierNames.split(',').map((s: string) => s.trim()).filter(Boolean)
          : [],
        variants: r.variants
          ? r.variants.split(',').map((v: string) => v.trim()).filter(Boolean)
          : []
      })),

      fileMetadata: this.buildFileMetadata(),

      options: {
        dryRun: true,
        createMissingCategories: this.createMissingCategories,
        createMissingSuppliers: this.createMissingSuppliers
      }
    };

    const formData = new FormData();

    formData.append(
      'payload',
      new Blob([JSON.stringify(payload)], { type: 'application/json' })
    );

    this.appendFilesToFormData(formData);

    this.submitEngine.executeMultipart({
      submitFn: fd => this.productService.bulkFullCreate(fd),
      formData,
      rows: this.rows.controls, // 🔥 CRITICAL
      dryRun: true,
      title: this.config.title,
      confirmLabel: 'Confirm Import',
      columns: this.config.previewColumns,
      onPreviewTransform: res => {
        if (this.config.mapPreviewRow && Array.isArray(res.data)) {
          res.data = res.data.map(r =>
            this.config.mapPreviewRow!(r)
          );
        }
      },
      onErrorsApplied: res => this.cacheErrors(res), // 🔥 match users
      onConfirmRetry: () => this.submitBulk(),
      onFinalSuccess: () => { }
    });
  }

  /* =========================================================
     FINAL COMMIT
  ========================================================= */

  submitBulk() {

    const payload = {

      products: this.rows.value.map((r: any) => ({
        ...r,
        supplierNames: r.supplierNames
          ? r.supplierNames.split(',').map((s: string) => s.trim()).filter(Boolean)
          : [],
        variants: r.variants
          ? r.variants.split(',').map((v: string) => v.trim()).filter(Boolean)
          : []
      })),

      fileMetadata: this.buildFileMetadata(),

      options: {
        dryRun: false,
        createMissingCategories: this.createMissingCategories,
        createMissingSuppliers: this.createMissingSuppliers
      }
    };

    const formData = new FormData();

    formData.append(
      'payload',
      new Blob([JSON.stringify(payload)], { type: 'application/json' })
    );

    this.appendFilesToFormData(formData);

    this.submitEngine.executeMultipart({
      submitFn: fd => this.productService.bulkFullCreate(fd),
      formData,
      rows: this.rows.controls,
      dryRun: false,
      title: this.config.title,
      columns: this.config.previewColumns,
      onPreviewTransform: res => {
        if (this.config.mapPreviewRow && Array.isArray(res.data)) {
          res.data = res.data.map(r =>
            this.config.mapPreviewRow!(r)
          );
        }
      },
      onErrorsApplied: res => this.cacheErrors(res),
      onFinalSuccess: () => this.dialogRef.close(true)
    });
  }
}