import {
    Component,
    OnInit,
    ChangeDetectorRef
} from '@angular/core';

import { CommonModule } from '@angular/common';

import {
    FormsModule,
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
import { FundingAccount } from '../../../../finance/ap/debts/models/funding-account.model';
import { Account } from '../../../../finance/accounts/models/account.models';
import { AccountsService } from '../../../../finance/accounts/services/accounts.service';
import { ApPaymentService } from '../../../../finance/ap/debts/services/ap-payment.service';
import { MatSelectModule } from '@angular/material/select';
import { MatOptionModule } from '@angular/material/core';

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
        MatTooltipModule,
        MatSelectModule,
        MatOptionModule,
        FormsModule
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

    fundingAccounts: FundingAccount[] = [];

    expenseAccounts: Account[] = [];

    private fundingAccountMap =
        new Map<string, string>();

    private expenseAccountMap =
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

    accountingDate =
        new Date()
            .toISOString()
            .substring(0, 10);

    autoPaySuppliers = false;

    supplierPaymentMethod:
        'CASH'
        | 'BANK'
        | 'MPESA'
        | null = null;

    autoPayOperationalExpenses = false;

    fundingAccountId:
        string | null = null;

    operationalExpenses: {
        expenseAccountId: string;
        description: string;
        amount: number;
    }[] = [];

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

        private accountsService:
            AccountsService,

        private paymentService:
            ApPaymentService,
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

        this.loadFundingAccounts();

        this.loadExpenseAccounts();

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

    get totalUnits(): number {
        return this.rows.controls.reduce(
            (sum, row) =>
                sum +
                Number(
                    row.value.unitsSupplied || 0
                ),
            0
        );
    }

    get totalInventoryValue(): number {
        return this.rows.controls.reduce(
            (sum, row) =>
                sum +
                (
                    Number(
                        row.value.unitsSupplied || 0
                    ) *
                    Number(
                        row.value.unitCost || 0
                    )
                ),
            0
        );
    }

    get totalOperationalExpenses(): number {
        return this.operationalExpenses.reduce(
            (sum, expense) =>
                sum +
                Number(
                    expense.amount || 0
                ),
            0
        );
    }

    get supplierFundingRequired(): number {
        return this.autoPaySuppliers
            ? this.totalInventoryValue
            : 0;
    }

    get expenseFundingRequired(): number {
        return this.autoPayOperationalExpenses
            ? this.totalOperationalExpenses
            : 0;
    }

    get totalFundingRequired(): number {
        return (
            this.supplierFundingRequired +
            this.expenseFundingRequired
        );
    }

    get selectedFundingAccount():
        FundingAccount | null {

        return (
            this.fundingAccounts.find(
                account =>
                    account.id ===
                    this.fundingAccountId
            ) || null
        );
    }

    get fundingShortfall(): number {
        const balance =
            this.selectedFundingAccount
                ?.balance ?? 0;

        return Math.max(
            0,
            this.totalFundingRequired -
            balance
        );
    }

    get fundingSurplus(): number {
        const balance =
            this.selectedFundingAccount
                ?.balance ?? 0;

        return Math.max(
            0,
            balance -
            this.totalFundingRequired
        );
    }

    get requiresAutoFunding(): boolean {
        return this.fundingShortfall > 0;
    }

    get totalDistinctProducts(): number {

        const products =
            new Set<string>();

        this.rows.controls.forEach(
            row => {

                const value =
                    String(
                        row.value.productName || ''
                    )
                        .trim()
                        .toUpperCase();

                if (value) {
                    products.add(value);
                }
            }
        );

        return products.size;
    }

    get totalDistinctVariants(): number {

        const variants =
            new Set<string>();

        this.rows.controls.forEach(
            row => {

                const value = [
                    row.value.productName,
                    row.value.classification
                ]
                    .join('||')
                    .trim()
                    .toUpperCase();

                variants.add(value);
            }
        );

        return variants.size;
    }

    get totalDistinctSuppliers(): number {

        const suppliers =
            new Set<string>();

        this.rows.controls.forEach(
            row => {

                const value =
                    String(
                        row.value.supplierName || ''
                    )
                        .trim()
                        .toUpperCase();

                if (value) {
                    suppliers.add(value);
                }
            }
        );

        return suppliers.size;
    }

    get totalDistinctBranches(): number {

        const branches =
            new Set<string>();

        this.rows.controls.forEach(
            row => {

                const value =
                    String(
                        row.value.branchCode || ''
                    )
                        .trim()
                        .toUpperCase();

                if (value) {
                    branches.add(value);
                }
            }
        );

        return branches.size;
    }

    get previewSupplierFunding(): number {

        return this.autoPaySuppliers
            ? this.totalInventoryValue
            : 0;
    }

    get previewExpenseFunding(): number {

        return this.autoPayOperationalExpenses
            ? this.totalOperationalExpenses
            : 0;
    }

    get previewFundingTotal(): number {

        return (
            this.previewSupplierFunding +
            this.previewExpenseFunding
        );
    }

    get estimatedVatAmount(): number {

        return this.rows.controls.reduce(
            (sum, row) => {

                const units =
                    Number(
                        row.value.unitsSupplied || 0
                    );

                const cost =
                    Number(
                        row.value.unitCost || 0
                    );

                const vatRate =
                    Number(
                        row.value.vatRate || 0
                    );

                const vatInclusive =
                    this.parseBoolean(
                        row.value.vatInclusive
                    );

                const lineTotal =
                    units * cost;

                if (!vatInclusive) {

                    return (
                        sum +
                        (
                            lineTotal *
                            vatRate /
                            100
                        )
                    );
                }

                const vat =
                    lineTotal -
                    (
                        lineTotal /
                        (
                            1 +
                            vatRate / 100
                        )
                    );

                return sum + vat;

            },
            0
        );
    }

    get estimatedNetCost(): number {

        return (
            this.totalInventoryValue -
            this.estimatedVatAmount
        );
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

    private loadFundingAccounts() {

        this.paymentService
            .fundingAccounts()
            .subscribe(accounts => {

                this.fundingAccounts = accounts;

                if (
                    !this.supplierPaymentMethod &&
                    !this.fundingAccountId
                ) {
                    const cashAccount =
                        accounts.find(account =>
                            account.name?.toUpperCase().includes('CASH')
                            ||
                            account.code?.toUpperCase().includes('CASH')
                        );

                    if (cashAccount) {
                        this.fundingAccountId =
                            cashAccount.id;

                        this.supplierPaymentMethod =
                            'CASH';
                    }
                }

                this.fundingAccountMap.clear();

                accounts.forEach(account => {

                    this.fundingAccountMap.set(
                        account.code
                            .trim()
                            .toUpperCase(),
                        account.id
                    );

                });

            });

    }

    private findFundingAccountByKeyword(
        keyword: string
    ): FundingAccount | undefined {
        return this.fundingAccounts.find(account =>
            account.name?.toUpperCase().includes(keyword)
            ||
            account.code?.toUpperCase().includes(keyword)
        );
    }

    onSupplierMethodChanged(): void {

        if (!this.supplierPaymentMethod) {
            return;
        }

        let account: FundingAccount | undefined;

        switch (this.supplierPaymentMethod) {

            case 'CASH':
                account =
                    this.findFundingAccountByKeyword('CASH');
                break;

            case 'BANK':
                account =
                    this.findFundingAccountByKeyword('BANK');
                break;

            case 'MPESA':
                account =
                    this.findFundingAccountByKeyword('MPESA')
                    ??
                    this.findFundingAccountByKeyword('M-PESA');
                break;
        }

        if (account) {
            this.fundingAccountId = account.id;
        }
    }

    onFundingAccountChanged(): void {

        const account =
            this.fundingAccounts.find(
                a => a.id === this.fundingAccountId
            );

        if (!account) {
            return;
        }

        const text = [
            account.code,
            account.name
        ]
            .join(' ')
            .toUpperCase();

        if (text.includes('MPESA') || text.includes('M-PESA')) {

            this.supplierPaymentMethod = 'MPESA';
            return;
        }

        if (text.includes('BANK')) {

            this.supplierPaymentMethod = 'BANK';
            return;
        }

        if (text.includes('CASH')) {

            this.supplierPaymentMethod = 'CASH';
            return;
        }
    }

    private loadExpenseAccounts() {

        this.accountsService
            .list({
                page: 0,
                size: 1000
            })
            .subscribe(result => {

                this.expenseAccounts =
                    result.content
                        .filter(account =>
                            account.type === 'EXPENSE'
                            &&
                            account.role !== 'COGS'
                            &&
                            account.role !== 'CORPORATE_TAX_EXPENSE'
                        );

                if (
                    this.expenseAccounts.length === 1
                ) {

                    const account =
                        this.expenseAccounts[0];

                    this.operationalExpenses =
                        this.operationalExpenses.map(
                            expense => ({
                                ...expense,
                                expenseAccountId:
                                    account.id
                            })
                        );
                }

                this.expenseAccountMap.clear();

                this.expenseAccounts
                    .forEach(account => {

                        this.expenseAccountMap.set(
                            account.code
                                .trim()
                                .toUpperCase(),
                            account.id
                        );

                    });

            });

    }

    addOperationalExpense() {

        const defaultAccountId =
            this.expenseAccounts.length === 1
                ? this.expenseAccounts[0].id
                : '';

        this.operationalExpenses.push({
            expenseAccountId:
                defaultAccountId,
            description: '',
            amount: 0
        });
    }

    removeOperationalExpense(
        index: number
    ) {

        this.operationalExpenses.splice(
            index,
            1
        );

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
                            ),
                        vatInclusive:
                            this.parseBoolean(
                                row.vatInclusive
                            ),
                        vatRate:
                            Number(
                                row.vatRate
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
                ),

            accountingDate:
                this.accountingDate,

            operationalExpenses:
                structuredClone(
                    this.operationalExpenses
                ),

            autoPaySuppliers:
                this.autoPaySuppliers,

            supplierPaymentMethod:
                this.supplierPaymentMethod
                ?? undefined,

            autoPayOperationalExpenses:
                this.autoPayOperationalExpenses,

            fundingAccountId:
                this.fundingAccountId
                ?? undefined
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

            const vatRate =
                Number(value.vatRate);

            if (
                isNaN(vatRate)
                || vatRate < 0
                || vatRate > 100
            ) {
                row.patchValue({
                    _error:
                        'VAT rate must be between 0 and 100.'
                });

                return;
            }

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

    private parseBoolean(value: any): boolean {
        if (typeof value === 'boolean') {
            return value;
        }

        if (typeof value === 'string') {
            return value.trim().toLowerCase() === 'true';
        }

        return !!value;
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
                        'app/stock'
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