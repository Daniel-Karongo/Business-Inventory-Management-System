import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReactiveFormsModule } from '@angular/forms';
import { MatDialogRef } from '@angular/material/dialog';

import { BulkImportFormComponent } from
    '../../../../../../../shared/bulk-import/base/bulk-import-form.component';
import { BulkImportSubmitEngineService } from
    '../../../../../../../shared/bulk-import/engine/bulk-import-submit-engine.service';
import { BulkImportShellComponent } from
    '../../../../../../../shared/bulk-import/shell/bulk-import-shell.component';

import { CategoryService } from '../../services/category.service';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { CATEGORY_BULK_IMPORT_CONFIG } from '../../services/category-bulk-import.config';
import { AuthService } from '../../../../../../auth/services/auth.service';

@Component({
    standalone: true,
    selector: 'app-category-bulk-import-dialog',
    templateUrl: './category-bulk-import-dialog.component.html',
    styleUrls: ['./category-bulk-import-dialog.component.scss'],
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
export class CategoryBulkImportDialogComponent
    extends BulkImportFormComponent<any, any, any>
    implements OnInit {

    config = CATEGORY_BULK_IMPORT_CONFIG;

    constructor(
        private categoryService: CategoryService,
        private submitEngine: BulkImportSubmitEngineService,
        private dialogRef: MatDialogRef<CategoryBulkImportDialogComponent>,
        private auth: AuthService
    ) {
        super();
    }

    ngOnInit() {
        this.initForm();
    }

    submit() {
        if (this.form.invalid) return;

        const branchId = this.getBranchId();

        const payload = {
            items: this.rows.controls.map(r => ({
                name: r.value.name,
                parentName: r.value.parentName || null,
                branchId: branchId
            })),
            options: {
                dryRun: this.form.value.dryRun,
                skipDuplicates: true
            }
        };

        this.submitEngine.execute({
            submitFn: req => this.categoryService.bulkImport(req),
            payload,
            rows: this.rows.controls,
            dryRun: this.form.value.dryRun,
            title: this.config.title,
            confirmLabel: this.config.confirmLabel,
            columns: this.config.previewColumns,
            onErrorsApplied: res => {
                this.cacheErrors(res);
            },
            onFinalSuccess: () => this.dialogRef.close(true),
            onConfirmRetry: () => {
                this.form.patchValue({ dryRun: false });
                this.submit();
            }
        });
    }

    private getBranchId(): string {
        const snapshot = this.auth.getSnapshot();

        if (!snapshot) {
            throw new Error('Auth context not initialized. Ensure auth.init() is called at app startup.');
        }

        if (!snapshot.branchId) {
            throw new Error('User has no branch assigned.');
        }

        return snapshot.branchId;
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