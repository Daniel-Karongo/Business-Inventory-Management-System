import {
    Component,
    OnInit,
    ChangeDetectorRef
} from '@angular/core';

import { CommonModule } from '@angular/common';

import {
    ReactiveFormsModule
} from '@angular/forms';

import {
    MatDialogRef
} from '@angular/material/dialog';

import {
    MatCheckboxModule
} from '@angular/material/checkbox';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatFormFieldModule
} from '@angular/material/form-field';

import {
    MatInputModule
} from '@angular/material/input';

import {
    MatIconModule
} from '@angular/material/icon';

import {
    MatPaginatorModule,
    PageEvent
} from '@angular/material/paginator';

import {
    BulkImportFormComponent
} from '../../../../../../../shared/bulk-import/base/bulk-import-form.component';

import {
    BulkImportShellComponent
} from '../../../../../../../shared/bulk-import/shell/bulk-import-shell.component';

import {
    BulkImportSubmitEngineService
} from '../../../../../../../shared/bulk-import/engine/bulk-import-submit-engine.service';

import {
    STOCK_ONBOARDING_BULK_IMPORT_CONFIG
} from './stock-onboarding-bulk-import.config';

import {
    StockOnboardingService
} from '../../services/stock-onboarding.service';

import {
    BranchService
} from '../../../../branches/services/branch.service';
import { MatTooltipModule } from '@angular/material/tooltip';
import { Router } from '@angular/router';

@Component({
    standalone: true,

    selector:
        'app-stock-onboarding-bulk-import-dialog',

    templateUrl:
        './stock-onboarding-bulk-import-dialog.component.html',

    styleUrls: [
        './stock-onboarding-bulk-import-dialog.component.scss'
    ],

    imports: [
        CommonModule,
        ReactiveFormsModule,
        BulkImportShellComponent,
        MatCheckboxModule,
        MatButtonModule,
        MatFormFieldModule,
        MatInputModule,
        MatIconModule,
        MatPaginatorModule,
        MatTooltipModule
    ]
})
export class
    StockOnboardingBulkImportDialogComponent
    extends BulkImportFormComponent<
        any,
        any,
        any
    >
    implements OnInit {

    config =
        STOCK_ONBOARDING_BULK_IMPORT_CONFIG;

    private branchMap =
        new Map<string, string>();

    /* ========================================================
       PAGINATION
    ======================================================== */

    readonly pageSizeOptions = [
        5,
        10,
        25,
        50
    ];

    pageSize = 10;

    currentPage = 0;

    pagedIndexes: number[] = [];

    constructor(
        private onboardingService:
            StockOnboardingService,
        private submitEngine:
            BulkImportSubmitEngineService,
        private dialogRef:
            MatDialogRef<
                StockOnboardingBulkImportDialogComponent
            >,
        private branchService:
            BranchService,
        private cdr:
            ChangeDetectorRef,
        private router:
            Router
    ) {
        super();
    }

    /* ========================================================
       INIT
    ======================================================== */

    ngOnInit(): void {

        this.initForm();

        this.rebuildPagedIndexes();

        this.branchService.getAllLegacy()
            .subscribe(branches => {

                this.branchMap.clear();

                branches.forEach(branch => {

                    if (
                        branch.branchCode &&
                        branch.id
                    ) {

                        this.branchMap.set(
                            branch.branchCode
                                .trim()
                                .toUpperCase(),
                            branch.id
                        );

                    }

                });

            });

    }

    /* ========================================================
       PAGINATION
    ======================================================== */

    private rebuildPagedIndexes() {

        const start =
            this.currentPage * this.pageSize;

        const end = Math.min(
            start + this.pageSize,
            this.rows.length
        );

        this.pagedIndexes = Array.from(
            { length: end - start },
            (_, i) => start + i
        );

    }

    get startIndex(): number {

        return this.currentPage * this.pageSize;

    }

    trackByRowIndex(
        _: number,
        rowIndex: number
    ): number {

        return rowIndex;

    }

    resetToFirstPage() {

        this.currentPage = 0;

    }

    onPageChange(
        event: PageEvent
    ) {

        this.currentPage =
            event.pageIndex;

        this.pageSize =
            event.pageSize;

        this.rebuildPagedIndexes();

    }

    private ensurePageVisibleForRow(
        rowIndex: number
    ) {

        this.currentPage = Math.floor(
            rowIndex / this.pageSize
        );

    }

    /* ========================================================
       ROWS
    ======================================================== */

    override addRow(
        data?: Partial<any>
    ) {

        super.addRow(data);

        this.rebuildPagedIndexes();

    }

    override removeRow(
        index: number
    ) {

        super.removeRow(index);

        const maxPage = Math.max(
            Math.ceil(
                this.rows.length / this.pageSize
            ) - 1,
            0
        );

        if (
            this.currentPage > maxPage
        ) {

            this.currentPage = maxPage;

        }

        this.rebuildPagedIndexes();

    }

    /* ========================================================
       ERROR NAVIGATION
    ======================================================== */

    protected override cacheErrors(
        _: any
    ) {

        const errors =
            this.errorRows;

        if (!errors.length) {
            return;
        }

        const targetRow =
            errors[0] - 1;

        this.ensurePageVisibleForRow(
            targetRow
        );

        this.rebuildPagedIndexes();

        queueMicrotask(() => {

            requestAnimationFrame(
                () => {

                    this.scroll.goToLine(
                        errors[0]
                    );

                }
            );

        });

    }

    protected override navigateToErrorRow(
        line: number
    ): void {

        const rowIndex =
            line - 1;

        this.ensurePageVisibleForRow(
            rowIndex
        );

        this.rebuildPagedIndexes();

        this.cdr.detectChanges();

        requestAnimationFrame(() => {

            this.scroll.goToLine(
                line
            );

        });

    }

    protected override notifyNoMoreErrors(): void {

        this.snackbar.open(
            'No more errors',
            'Close',
            {
                duration: 3000
            }
        );

    }

    /* ========================================================
       BRANCH
    ======================================================== */

    private resolveBranchId(
        branchCode?: string
    ): string {

        if (!branchCode?.trim()) {

            throw new Error(
                'branchCode is required'
            );

        }

        const normalized =
            branchCode
                .trim()
                .toUpperCase();

        const branchId =
            this.branchMap.get(
                normalized
            );

        if (!branchId) {

            throw new Error(
                `Branch not found for code: ${branchCode}`
            );

        }

        return branchId;

    }

    /* ========================================================
       PAYLOAD
    ======================================================== */

    private buildGroupedPayload() {

        const grouped =
            new Map<string, any[]>();

        for (
            const row
            of this.rows.controls
        ) {

            const value =
                row.value;

            const key = [
                value.productName
                    ?.trim()
                    ?.toUpperCase(),

                value.classification
                    ?.trim()
                    ?.toUpperCase()
            ].join('||');

            if (
                !grouped.has(key)
            ) {

                grouped.set(
                    key,
                    []
                );

            }

            grouped
                .get(key)!
                .push(value);

        }

        return {

            items:
                Array.from(
                    grouped.values()
                )
                    .map(
                        rows =>
                            this.buildGroupedItem(
                                rows
                            )
                    ),

            options: {

                dryRun:
                    this.form.value
                        .dryRun

            }

        };

    }

    private buildGroupedItem(
        rows: any[]
    ) {

        const first =
            rows[0];

        const packagingMap =
            new Map<string, any>();

        const pricingMap =
            new Map<string, any>();

        const supplierMap =
            new Map<string, any>();

        for (const row of rows) {

            const packagingKey =
                String(
                    row.packagingName || ''
                )
                    .trim()
                    .toUpperCase();

            if (
                !packagingMap.has(
                    packagingKey
                )
            ) {

                packagingMap.set(
                    packagingKey,
                    {
                        name:
                            row.packagingName,

                        units:
                            Number(
                                row.packagingUnits
                            )
                    }
                );

            }

            const pricingKey =
                String(
                    row.packagingName || ''
                )
                    .trim()
                    .toUpperCase();

            if (
                !pricingMap.has(
                    pricingKey
                )
            ) {

                pricingMap.set(
                    pricingKey,
                    {
                        packagingName:
                            row.packagingName,

                        sellingPrice:
                            Number(
                                row.sellingPrice
                            )
                    }
                );

            }

            const supplierKey = [

                String(
                    row.supplierId || ''
                )
                    .trim()
                    .toUpperCase(),

                String(
                    row.supplierName || ''
                )
                    .trim()
                    .toUpperCase(),

                String(
                    row.packagingName || ''
                )
                    .trim()
                    .toUpperCase(),

                Number(
                    row.unitCost
                )

            ].join('||');

            if (
                !supplierMap.has(
                    supplierKey
                )
            ) {

                supplierMap.set(
                    supplierKey,
                    {
                        supplierId:
                            row.supplierId || undefined,

                        supplierName:
                            row.supplierName || undefined,

                        createSupplierIfMissing:
                            !!row.supplierName,

                        packagingName:
                            row.packagingName,

                        unitsSupplied:
                            Number(
                                row.unitsSupplied
                            ),

                        unitCost:
                            Number(
                                row.unitCost
                            )
                    }
                );

            }

        }

        return {

            productName:
                first.productName,

            categoryId:
                first.categoryId
                || undefined,

            newCategoryName:
                first.categoryName
                || undefined,

            classification:
                first.classification,

            branchId:
                this.resolveBranchId(
                    first.branchCode
                ),

            reference:
                this.normalizeReference(
                    first.reference
                ),

            note:
                first.note,

            minimumPercentageProfit:
                first.minimumPercentageProfit,

            packagings:
                Array.from(
                    packagingMap.values()
                ),

            pricing:
                Array.from(
                    pricingMap.values()
                ),

            suppliers:
                Array.from(
                    supplierMap.values()
                )

        };

    }

    private validateRowConsistency() {

        // clear previous errors
        this.rows.controls.forEach(row => {

            row.patchValue(
                { _error: '' },
                { emitEvent: false }
            );

        });

        const packagingMap =
            new Map<
                string,
                {
                    units: number;
                    rows: number[];
                }
            >();

        this.rows.controls.forEach((row, index) => {

            const value = row.value;

            const groupingKey = [

                String(value.productName || '')
                    .trim()
                    .toUpperCase(),

                String(value.classification || '')
                    .trim()
                    .toUpperCase(),

                String(value.packagingName || '')
                    .trim()
                    .toUpperCase()

            ].join('||');

            const packagingUnits =
                Number(value.packagingUnits);

            // first occurrence
            if (!packagingMap.has(groupingKey)) {

                packagingMap.set(
                    groupingKey,
                    {
                        units: packagingUnits,
                        rows: [index]
                    }
                );

                return;

            }

            const existing =
                packagingMap.get(groupingKey)!;

            // same units → valid duplicate
            if (existing.units === packagingUnits) {

                existing.rows.push(index);

                return;

            }

            // conflict detected
            const affectedRows = [
                ...existing.rows,
                index
            ];

            affectedRows.forEach(rowIndex => {

                const affectedRow =
                    this.rows.at(rowIndex);

                affectedRow.patchValue({

                    _error:
                        `Conflicting packaging units detected for packaging "${value.packagingName}". Multiple rows define different packaging units within the same onboarding group (product classification).`

                });

            });

        });

    }

    afterRowsImported() {

        this.validateRowConsistency();

        const errors =
            this.errorRows;

        if (!errors.length) {

            return;

        }

        const targetRow =
            errors[0] - 1;

        this.ensurePageVisibleForRow(
            targetRow
        );

        this.rebuildPagedIndexes();

        this.cdr.detectChanges();

        this.snackbar.open(

            `⚠️ ${errors.length} validation error${errors.length === 1 ? '' : 's'} found in the import.`,

            'Close',

            {
                duration: 9000,
                horizontalPosition: 'center',
                verticalPosition: 'bottom'
            }

        );

        requestAnimationFrame(() => {

            this.scroll.goToLine(
                errors[0]
            );

        });

    }

    private normalizeReference(
        reference?: string | null,
        type = 'RECEIPT'
    ): string {

        const value = reference?.trim();

        if (!value) {
            return `${type}:${crypto.randomUUID()}`;
        }

        // already valid TYPE:UUID
        if (
            value.includes(':') &&
            value.split(':').length === 2
        ) {
            return value;
        }

        return `${type}:${crypto.randomUUID()}`;
    }

    /* ========================================================
       SUBMIT
    ======================================================== */

    submit() {

        if (
            this.form.invalid
        ) {
            return;
        }

        this.validateRowConsistency();

        const hasErrors =
            this.rows.controls.some(
                row => !!row.value._error
            );

        if (hasErrors) {

            this.cacheErrors(null);

            this.snackbar.open(
                'Please resolve validation errors before importing.',
                'Close',
                { duration: 5000 }
            );

            return;
        }

        const payload =
            this.buildGroupedPayload();

        this.submitEngine.execute({

            submitFn:
                req =>
                    this.onboardingService
                        .bulkCreate(req),

            payload,

            rows:
                this.rows.controls,

            dryRun:
                this.form.value
                    .dryRun,

            title:
                this.config.title,

            confirmLabel:
                this.config.confirmLabel,

            columns:
                this.config
                    .previewColumns,

            setLoading:
                loading => {

                    this.submitting =
                        loading;

                    this.cdr.detectChanges();

                },

            onPreviewTransform:
                result => {

                    result.data =
                        result.data.map(
                            row =>
                                this.config
                                    .mapPreviewRow?.(
                                        row
                                    ) || row
                        );

                },

            onErrorsApplied:
                result =>
                    this.cacheErrors(
                        result
                    ),

            onFinalSuccess:
                async () => {

                    this.dialogRef.close(true);

                    await this.router.navigate([
                        'app/inventory'
                    ]);
                },

            onConfirmRetry: () => {

                this.form.patchValue({
                    dryRun: false
                });

                this.submit();
            }

        });

    }

    /* ========================================================
       IMPORTS
    ======================================================== */

    async importExcel(
        file:
            File | undefined
    ) {

        if (!file) {
            return;
        }

        const mode =
            await this.confirmMerge();

        if (!mode) {
            return;
        }

        super.importExcelFile(
            file,
            undefined,
            mode
        );

        this.currentPage = 0;

        this.rebuildPagedIndexes();

    }

    async importCsv(
        file:
            File | undefined
    ) {

        if (!file) {
            return;
        }

        const mode =
            await this.confirmMerge();

        if (!mode) {
            return;
        }

        super.importCsvFile(
            file,
            mode
        );

        this.currentPage = 0;

        this.rebuildPagedIndexes();

    }

    /* ========================================================
       CLOSE
    ======================================================== */

    close() {

        this.dialogRef.close();

    }

}