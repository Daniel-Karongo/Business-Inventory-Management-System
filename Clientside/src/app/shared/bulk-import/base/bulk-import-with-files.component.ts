import { FormArray } from '@angular/forms';

import { BulkImportFormComponent } from './bulk-import-form.component';
import { BulkAssignedFile } from '../files/bulk-file.model';
import { BulkFileFactoryService } from '../files/bulk-file-factory.service';
import { BulkFileLifecycleService } from '../files/bulk-file-lifecycle.service';
import { BulkFileImportEngine } from '../files/bulk-file-import.engine';
import { Directive, inject, OnDestroy } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { BulkFileRowAssignmentDialogComponent }
    from '../files/bulk-file-row-assignment-dialog.component';
import { MatSnackBar } from '@angular/material/snack-bar';
import { BulkCameraCaptureService } from '../camera/bulk-camera-capture.service';

@Directive()
export abstract class BulkImportWithFilesComponent<
    TRow,
    TItem,
    TResult
> extends BulkImportFormComponent<TRow, TItem, TResult>
    implements OnDestroy {

    protected override dialog = inject(MatDialog);
    protected override snackbar = inject(MatSnackBar);

    allFiles: BulkAssignedFile[] = [];

    constructor(
        protected fileFactory: BulkFileFactoryService,
        protected fileLifecycle: BulkFileLifecycleService,
        protected zipEngine: BulkFileImportEngine,
        protected camera: BulkCameraCaptureService
    ) {
        super();
    }

    /* ============================================
       ROW EXTENSION
    ============================================ */

    override addRow(data?: any) {

        super.addRow(data);

        const index = this.rows.length - 1;
        const control = this.rows.at(index);

        if (!control.contains('fileIds')) {
            control.addControl('fileIds', this.fb.control([]));
        }
    }

    getUnassignedFiles(): BulkAssignedFile[] {
        return this.allFiles.filter(f =>
            f.assignedRowIndexes.length === 0
        );
    }

    protected override clearRowsInternal() {

        super.clearRowsInternal();

        this.fileLifecycle.revokeFiles(this.allFiles);
        this.allFiles = [];
    }

    override ngOnDestroy() {
        this.fileLifecycle.revokeFiles(this.allFiles);
        super.ngOnDestroy?.();
    }

    /* ============================================
       FILE HELPERS
    ============================================ */

    openAssignmentDialog(input: BulkAssignedFile | BulkAssignedFile[]) {

        const files = Array.isArray(input) ? input : [input];

        const rows = this.rows.value.map((r: any, index: number) => ({
            index,
            name: r.name
        }));

        const ref = this.dialog.open(
            BulkFileRowAssignmentDialogComponent,
            {
                data: { files, rows },
                maxWidth: '95vw'
            }
        );

        ref.afterClosed().subscribe(result => {

            if (!result) return;

            files.forEach(file => {

                file.assignedRowIndexes = result;
                file.assignToEntity = true;

                result.forEach((rowIndex: number) => {

                    const variantsControl =
                        this.rows.at(rowIndex).get('variants')?.value ?? '';

                    const variants = variantsControl
                        .split(',')
                        .map((v: string) => v.trim().toUpperCase())
                        .filter(Boolean);

                    // When assigning to a row → always assign to entity
                    file.assignToEntity = true;

                    if (!file.rowVariantMap[rowIndex]) {

                        if (variants.includes('STANDARD')) {
                            file.rowVariantMap[rowIndex] = ['STANDARD'];
                        } else if (variants.length) {
                            file.rowVariantMap[rowIndex] = [variants[0]];
                        } else {
                            file.rowVariantMap[rowIndex] = [];
                        }
                    }

                });

                Object.keys(file.rowVariantMap).forEach(key => {
                    const rowIndex = Number(key);
                    if (!result.includes(rowIndex)) {
                        delete file.rowVariantMap[rowIndex];
                    }
                });

                this.rows.controls.forEach((control, index) => {

                    const ids = control.get('fileIds')?.value ?? [];

                    if (result.includes(index)) {

                        if (!ids.includes(file.id)) {
                            control.patchValue({
                                fileIds: [...ids, file.id]
                            });
                        }

                    } else {

                        control.patchValue({
                            fileIds: ids.filter((id: string) => id !== file.id)
                        });
                    }
                });

                files.forEach(f => f);
            });
        });
    }

    getRowFiles(rowIndex: number): BulkAssignedFile[] {
        return this.allFiles.filter(f =>
            f.assignedRowIndexes.includes(rowIndex)
        );
    }

    onFileRemove(fileId: string) {

        const file = this.allFiles.find(f => f.id === fileId);
        if (!file) return;

        file.assignedRowIndexes = [];
    }

    onRowFilesSelected(rowIndex: number, list: FileList | null) {

        if (!list) return;

        Array.from(list).forEach(file => {

            const assigned =
                this.fileFactory.createAssignedFile(file);

            // Assign to row
            assigned.assignedRowIndexes = [rowIndex];

            // Assign to entity
            assigned.assignToEntity = true;

            // Assign default variant
            const variantsControl =
                this.rows.at(rowIndex).get('variants')?.value ?? '';

            const variants = variantsControl
                .split(',')
                .map((v: string) => v.trim().toUpperCase())
                .filter(Boolean);

            if (variants.includes('STANDARD')) {
                assigned.rowVariantMap[rowIndex] = ['STANDARD'];
            } else if (variants.length) {
                assigned.rowVariantMap[rowIndex] = [variants[0]];
            } else {
                assigned.rowVariantMap[rowIndex] = [];
            }

            this.allFiles.push(assigned);

            const row = this.rows.at(rowIndex);
            const ids = row.get('fileIds')?.value ?? [];

            row.patchValue({
                fileIds: [...ids, assigned.id]
            });

        });
    }

    /* ============================================
       ZIP IMPORT
    ============================================ */

    async importZip(file: File | undefined) {

        if (!file) return;

        if (this.allFiles.length > 0) {

            const mode = await this.confirmMerge();
            if (!mode) return;

            if (mode === 'replace') {
                this.fileLifecycle.revokeFiles(this.allFiles);
                this.allFiles = [];
            }
        }

        const rowsForMatching =
            this.rows.value.map((r: any) => ({
                name: r.name,
                variants: r.variants
                    ? r.variants.split(',').map((v: string) => v.trim())
                    : []
            }));

        const result =
            await this.zipEngine.parseZipAndMatch(
                file,
                rowsForMatching
            );

        let uploadedCount = 0;
        let duplicateCount = 0;

        result.entries.forEach(entry => {

            const alreadyExists = this.allFiles.some(f =>
                f.file.name === entry.file.name &&
                f.file.size === entry.file.size
            );

            if (alreadyExists) {
                duplicateCount++;
                return;
            }

            const assigned =
                this.fileFactory.createAssignedFile(entry.file);

            if (entry.matchedRowIndex !== undefined) {

                const rowIndex = entry.matchedRowIndex;

                assigned.assignedRowIndexes = [rowIndex];
                assigned.assignToEntity = true;

                const variantsControl =
                    this.rows.at(rowIndex).get('variants')?.value ?? '';

                const variants = variantsControl
                    .split(',')
                    .map((v: string) => v.trim().toUpperCase())
                    .filter(Boolean);

                if (variants.includes('STANDARD')) {
                    assigned.rowVariantMap[rowIndex] = ['STANDARD'];
                } else if (variants.length) {
                    assigned.rowVariantMap[rowIndex] = [variants[0]];
                } else {
                    assigned.rowVariantMap[rowIndex] = [];
                }

                const row =
                    this.rows.at(rowIndex);

                const ids =
                    row.get('fileIds')?.value ?? [];

                row.patchValue({
                    fileIds: [...ids, assigned.id]
                });

            } else {
                assigned.assignToEntity = false;
            }

            this.allFiles.push(assigned);
            uploadedCount++;
        });

        if (uploadedCount || duplicateCount) {

            const parts = [];

            if (uploadedCount) {
                parts.push(`${uploadedCount} file${uploadedCount > 1 ? 's' : ''} uploaded`);
            }

            if (duplicateCount) {
                parts.push(`${duplicateCount} duplicate${duplicateCount > 1 ? 's' : ''} skipped`);
            }

            this.snackbar.open(
                `✅ ${parts.join(', ')}`,
                'Close',
                { duration: 4000 }
            );
        }
    }

    /* ============================================
       FILE METADATA BUILDER
    ============================================ */

    protected buildFileMetadata() {

        return this.allFiles.map(f => ({
            id: f.id,
            fileName: f.file.name,
            assignedRowIndexes: f.assignedRowIndexes,
            rowVariantMap: f.rowVariantMap,
            assignToEntity: f.assignToEntity,
            description: f.description
        }));
    }

    protected appendFilesToFormData(formData: FormData) {
        this.allFiles.forEach(f => {
            formData.append('files', f.file);
        });
    }

    protected async capturePhotoForRow(
        rowIndex: number,
        entityType: string
    ) {

        const files = await this.camera.capture(entityType, rowIndex);
        if (!files?.length) return;

        files.forEach(file => {

            const assigned =
                this.fileFactory.createAssignedFile(file);

            assigned.assignedRowIndexes = [rowIndex];
            assigned.assignToEntity = true;

            // 🔥 Assign to STANDARD / first variant
            const variantsControl =
                this.rows.at(rowIndex).get('variants')?.value ?? '';

            const variants = variantsControl
                .split(',')
                .map((v: string) => v.trim().toUpperCase())
                .filter(Boolean);

            if (variants.length) {
                assigned.rowVariantMap[rowIndex] = [variants[0]];
            } else {
                assigned.rowVariantMap[rowIndex] = ['STANDARD'];
            }

            this.allFiles.push(assigned);

            const row = this.rows.at(rowIndex);
            const ids = row.get('fileIds')?.value ?? [];

            row.patchValue({
                fileIds: [...ids, assigned.id]
            });
        });
    }
}