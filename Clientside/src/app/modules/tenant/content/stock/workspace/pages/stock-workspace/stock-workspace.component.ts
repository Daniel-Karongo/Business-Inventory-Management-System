import {
    BreakpointObserver
} from '@angular/cdk/layout';

import {
    CommonModule
} from '@angular/common';

import {
    Component,
    OnInit,
    TemplateRef,
    ViewChild,
    inject
} from '@angular/core';

import {
    FormsModule
} from '@angular/forms';

import {
    DomSanitizer,
    SafeUrl
} from '@angular/platform-browser';

import {
    Router,
    RouterModule
} from '@angular/router';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatCheckboxModule
} from '@angular/material/checkbox';

import {
    MatDialog,
    MatDialogModule
} from '@angular/material/dialog';

import {
    MatFormFieldModule
} from '@angular/material/form-field';

import {
    MatIconModule
} from '@angular/material/icon';

import {
    MatInputModule
} from '@angular/material/input';

import {
    MatPaginatorModule,
    PageEvent
} from '@angular/material/paginator';

import {
    MatSelectModule
} from '@angular/material/select';

import {
    MatTableModule
} from '@angular/material/table';

import {
    MatTooltipModule
} from '@angular/material/tooltip';

import {
    BehaviorSubject,
    combineLatest,
    debounceTime,
    distinctUntilChanged,
    switchMap,
    tap
} from 'rxjs';

import {
    PageShellComponent
} from '../../../../../../../shared/layout/page-shell/page-shell.component';

import {
    StockWorkspaceCardField
} from '../../models/stock-workspace-card-field.model';

import {
    StockWorkspaceColumn
} from '../../models/stock-workspace-column.model';

import {
    StockWorkspaceDensity,
    StockWorkspaceViewMode
} from '../../models/stock-workspace-view.model';

import {
    StockWorkspaceRow
} from '../../models/stock-item.model';

import {
    StockWorkspaceService
} from '../../services/stock-workspace.service';

import {
    BranchMinimalDTO
} from '../../../../branches/models/branch.model';

import {
    BranchService
} from '../../../../branches/services/branch.service';

import {
    SupplierMinimalDTO
} from '../../../../suppliers/models/supplier.model';

import {
    SupplierService
} from '../../../../suppliers/services/supplier.service';

import {
    CategoryService
} from '../../../categories/services/category.service';

import {
    ProductService
} from '../../../products/parent/services/product.service';

import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import {
    FileViewerDialog
} from '../../../../../../../shared/components/file-viewer/file-viewer.component';



import { ProductSelectorDialogComponent } from '../../../../sales/dialogs/product-selector-dialog/product-selector-dialog.component';
import { AdjustStockDialogComponent } from '../../../inventory/components/adjust-stock-dialog/adjust-stock-dialog.component';
import { InventoryBulkImportDialogComponent } from '../../../inventory/components/inventory-bulk-import-dialog/inventory-bulk-import-dialog.component';
import { ReceiveNewProductDialogComponent } from '../../../inventory/components/receive-new-product-dialog/receive-new-product-dialog.component';
import { ReceiveStockDialogComponent } from '../../../inventory/components/receive-stock-dialog/receive-stock-dialog.component';
import { TransferStockDialogComponent } from '../../../inventory/components/transfer-stock-dialog/transfer-stock-dialog.component';
import { Product } from '../../../models/product.model';
import { ProductVariantService } from '../../../products/variant/services/product-variant.service';
import { ProductVariant } from '../../../models/product-variant.model';
import { VariantFormComponent } from '../../../products/variant/components/variant-form/variant-form.component';
import { VariantImagesDialogComponent } from '../../../products/variant/components/variant-images/variant-images-dialog.component';
import { VariantBarcodeDialogComponent } from '../../../products/variant/components/variant-barcode/variant-barcode-dialog.component';
import { VariantDetailsDialogComponent } from '../../../products/variant/components/variant-details/variant-details-dialog.component';

@Component({
    selector: 'app-stock-workspace',
    standalone: true,
    imports: [
        CommonModule,
        FormsModule,
        RouterModule,
        PageShellComponent,
        MatTableModule,
        MatButtonModule,
        MatCheckboxModule,
        MatIconModule,
        MatPaginatorModule,
        MatTooltipModule,
        MatFormFieldModule,
        MatInputModule,
        MatSelectModule,
        MatDialogModule,
        MatSnackBarModule
    ],
    templateUrl:
        './stock-workspace.component.html',
    styleUrls: [
        './stock-workspace.component.scss'
    ]
})
export class StockWorkspaceComponent
    implements OnInit {

    private workspace =
        inject(StockWorkspaceService);

    private breakpoint =
        inject(BreakpointObserver);

    private branchService =
        inject(BranchService);

    private categoryService =
        inject(CategoryService);

    private supplierService =
        inject(SupplierService);

    private router =
        inject(Router);

    private productService =
        inject(ProductService);

    private sanitizer =
        inject(DomSanitizer);

    private dialog =
        inject(MatDialog);

    private snackBar =
        inject(MatSnackBar);

    private variantService =
        inject(ProductVariantService);

    /* =========================================================
       UI
    ========================================================= */

    loading = true;

    isMobile = false;

    filtersVisible = true;

    viewMode:
        StockWorkspaceViewMode =
        'table';

    density:
        StockWorkspaceDensity =
        'compact';

    /* =========================================================
       DATA
    ========================================================= */

    rows:
        StockWorkspaceRow[] = [];

    total = 0;

    branches:
        BranchMinimalDTO[] = [];

    categories:
        any[] = [];

    suppliers:
        SupplierMinimalDTO[] = [];

    thumbnails =
        new Map<
            string,
            SafeUrl
        >();

    /* =========================================================
       FILTERS
    ========================================================= */

    private page$ =
        new BehaviorSubject<number>(0);

    private size$ =
        new BehaviorSubject<number>(25);

    private keyword$ =
        new BehaviorSubject<string>('');

    private branchId$ =
        new BehaviorSubject<
            string | null
        >(null);

    private categoryId$ =
        new BehaviorSubject<
            number | null
        >(null);

    private supplierId$ =
        new BehaviorSubject<
            string | null
        >(null);

    sortField$ =
        new BehaviorSubject<string>(
            'updatedAt'
        );

    sortDirection$ =
        new BehaviorSubject<
            'asc' | 'desc'
        >('desc');

    /* =========================================================
       SELECTION
    ========================================================= */

    selectedIds =
        new Set<string>();

    /* =========================================================
       TABLE CONFIG
    ========================================================= */

    columns:
        StockWorkspaceColumn<StockWorkspaceRow>[] = [
            {
                id: 'product',
                label: 'Product',
                sortable: true,
                value: x =>
                    x.productName
            },
            {
                id: 'variant',
                label: 'Variant',
                sortable: true,
                value: x =>
                    x.variantName
            },
            {
                id: 'variantSku',
                label: 'SKU',
                sortable: true,
                value: x =>
                    x.sku
            },
            {
                id: 'branch',
                label: 'Branch',
                sortable: true,
                value: x =>
                    x.branchName
            },
            {
                id: 'onHand',
                label: 'On Hand',
                sortable: true,
                value: x =>
                    x.quantityOnHand
            },
            {
                id: 'reserved',
                label: 'Reserved',
                sortable: true,
                value: x =>
                    x.quantityReserved
            },
            {
                id: 'available',
                label: 'Available',
                sortable: true,
                value: x =>
                    x.quantityAvailable
            },
            {
                id: 'batches',
                label: 'Batches',
                sortable: true,
                value: x =>
                    x.batchCount
            },
            {
                id: 'value',
                label: 'Value',
                sortable: true,
                value: x =>
                    x.inventoryValue
            },
            {
                id: 'updated',
                label: 'Updated',
                sortable: true,
                value: x =>
                    x.updatedAt
            }
        ];

    cardFields:
        StockWorkspaceCardField<StockWorkspaceRow>[] = [];

    /* =========================================================
       TEMPLATES
    ========================================================= */

    @ViewChild(
        'cardTemplate',
        { static: false }
    )
    cardTemplate?:
        TemplateRef<any>;

    @ViewChild(
        'tableCellTemplate',
        { static: false }
    )
    tableCellTemplate?:
        TemplateRef<any>;

    /* =========================================================
       INIT
    ========================================================= */

    ngOnInit(): void {

        this.breakpoint
            .observe([
                '(max-width: 640px)'
            ])
            .subscribe(result => {

                this.isMobile =
                    result.matches;

                if (
                    this.isMobile
                ) {
                    this.viewMode =
                        'grid';
                }
            });

        this.loadSupportingData();

        combineLatest([
            this.page$,
            this.size$,

            this.keyword$
                .pipe(
                    debounceTime(350),
                    distinctUntilChanged()
                ),

            this.branchId$,
            this.categoryId$,
            this.supplierId$,
            this.sortField$,
            this.sortDirection$
        ])
            .pipe(

                tap(() => {
                    this.loading = true;
                }),

                switchMap(([
                    page,
                    size,
                    keyword,
                    branchId,
                    categoryId,
                    supplierId,
                    sortBy,
                    direction
                ]) => {

                    return this.workspace.search({
                        page,
                        size,
                        keyword,

                        branchId:
                            branchId
                            ?? undefined,

                        categoryId:
                            categoryId
                            ?? undefined,

                        supplierId:
                            supplierId
                            ?? undefined,

                        sortBy,
                        direction
                    });
                })
            )
            .subscribe({

                next: result => {

                    this.rows =
                        result.content;

                    this.total =
                        result.totalElements;

                    this.loadThumbnails();

                    this.loading =
                        false;
                },

                error: () => {
                    this.loading =
                        false;
                }
            });
    }

    /* =========================================================
       DISPLAYED COLUMNS
    ========================================================= */

    get displayedColumns(): string[] {
        const cols: string[] = [];

        const hasVariants =
            this.rows.some(
                row => row.type === 'VARIANT'
            );

        const hasInventory =
            this.rows.some(
                row => row.type === 'INVENTORY'
            );

        cols.push('select');

        for (const column of this.columns) {

            /* =========================================
               VARIANT-LEVEL COLUMNS
            ========================================= */
            if (
                ['variant', 'variantSku'].includes(column.id) &&
                !hasVariants
            ) {
                continue;
            }

            /* =========================================
               INVENTORY-LEVEL COLUMNS
            ========================================= */
            if (
                [
                    'branch',
                    'onHand',
                    'reserved',
                    'available',
                    'batches',
                    'value'
                ].includes(column.id) &&
                !hasInventory
            ) {
                continue;
            }

            cols.push(column.id);
        }

        cols.push('actions');

        return cols;
    }

    get showBranchFilter(): boolean {
        return this.branches.length > 1;
    }

    get showCategoryFilter(): boolean {
        return this.categories.length > 1;
    }

    get showSupplierFilter(): boolean {
        return this.suppliers.length > 1;
    }

    /* =========================================================
       HIERARCHY
    ========================================================= */

    rowExpandable = (
        row: StockWorkspaceRow
    ) => {
        if (row.type === 'PRODUCT') {
            return (row.totalVariants ?? 0) > 0;
        }

        if (row.type === 'VARIANT') {
            return !!row.branchId || this.branches.length > 0;
        }

        return false;
    };

    rowExpanded = (
        row: StockWorkspaceRow
    ) =>
        !!row.expanded;

    rowLevel = (
        row: StockWorkspaceRow
    ) =>
        row.level ?? 0;

    toggleExpanded = (
        row: StockWorkspaceRow
    ) => {

        if (
            row.loadingChildren
        ) {
            return;
        }

        /* =========================================
           VARIANTS REQUIRE BRANCH FILTER
        ========================================= */
        if (
            row.type === 'VARIANT' &&
            !this.branchId$.value
        ) {

            this.snackBar.open(
                'Select a branch to load inventory.',
                'OK',
                {
                    duration: 3000
                }
            );

            return;
        }

        this.workspace
            .toggleExpanded(
                row
            )
            .subscribe(() => {

                this.rows = [
                    ...this.workspace
                        .getVisibleRows()
                ];
            });
    };

    expandable(
        row: StockWorkspaceRow
    ) {

        return this.rowExpandable(
            row
        );
    }

    expanded(
        row: StockWorkspaceRow
    ) {

        return this.rowExpanded(
            row
        );
    }

    rowLevelValue(
        row: StockWorkspaceRow
    ) {

        return this.rowLevel(
            row
        );
    }

    toggleRowExpanded(
        row: StockWorkspaceRow,
        event?: Event
    ) {

        event?.stopPropagation();

        this.toggleExpanded(
            row
        );
    }

    /* =========================================================
       FILTERS
    ========================================================= */

    setSearch(
        value: string
    ) {

        this.page$.next(0);

        this.keyword$.next(
            value
        );
    }

    setBranch(
        value:
            string | null
    ) {

        this.page$.next(0);

        this.branchId$.next(
            value
        );
    }

    setCategory(
        value:
            number | null
    ) {

        this.page$.next(0);

        this.categoryId$.next(
            value
        );
    }

    setSupplier(
        value:
            string | null
    ) {

        this.page$.next(0);

        this.supplierId$.next(
            value
        );
    }

    toggleFilters() {

        this.filtersVisible =
            !this.filtersVisible;
    }

    /* =========================================================
       SORTING
    ========================================================= */

    sort(
        field: string
    ) {

        if (
            this.sortField$.value
            === field
        ) {

            this.sortDirection$.next(

                this.sortDirection$
                    .value === 'asc'
                    ? 'desc'
                    : 'asc'
            );

            return;
        }

        this.sortField$.next(
            field
        );

        this.sortDirection$.next(
            'asc'
        );
    }

    triggerSort(
        column:
            StockWorkspaceColumn<StockWorkspaceRow>
    ) {

        if (
            !column.sortable
        ) {
            return;
        }

        this.sort(
            column.id
        );
    }

    getSortIcon(
        column:
            StockWorkspaceColumn<StockWorkspaceRow>
    ): string {

        if (
            this.sortField$.value
            !== column.id
        ) {
            return 'unfold_more';
        }

        return this.sortDirection$
            .value === 'asc'
            ? 'arrow_upward'
            : 'arrow_downward';
    }

    /* =========================================================
       PAGINATION
    ========================================================= */

    pageChange(
        event: PageEvent
    ) {

        this.page$.next(
            event.pageIndex
        );

        this.size$.next(
            event.pageSize
        );
    }

    /* =========================================================
       VIEW
    ========================================================= */

    setView(
        mode:
            StockWorkspaceViewMode
    ) {

        this.viewMode =
            mode;
    }

    toggleDensity() {

        this.density =
            this.density
                === 'compact'
                ? 'comfortable'
                : 'compact';
    }

    /* =========================================================
       SELECTION
    ========================================================= */

    toggleSelection(
        row: StockWorkspaceRow
    ) {

        const id =
            this.rowId(
                row
            );

        if (
            this.selectedIds.has(
                id
            )
        ) {

            this.selectedIds.delete(
                id
            );

        } else {

            this.selectedIds.add(
                id
            );
        }

        this.selectedIds =
            new Set(
                this.selectedIds
            );
    }

    toggleAll() {

        const rows =
            this.selectableRows();

        const allSelected =
            rows.every(
                row =>
                    this.selectedIds.has(
                        this.rowId(
                            row
                        )
                    )
            );

        if (
            allSelected
        ) {

            for (
                const row
                of rows
            ) {

                this.selectedIds.delete(
                    this.rowId(
                        row
                    )
                );
            }

        } else {

            for (
                const row
                of rows
            ) {

                this.selectedIds.add(
                    this.rowId(
                        row
                    )
                );
            }
        }

        this.selectedIds =
            new Set(
                this.selectedIds
            );
    }

    clearSelection() {

        this.selectedIds.clear();

        this.selectedIds =
            new Set(
                this.selectedIds
            );
    }

    allSelected():
        boolean {

        const rows =
            this.selectableRows();

        if (
            !rows.length
        ) {
            return false;
        }

        return rows.every(
            row =>
                this.selectedIds.has(
                    this.rowId(
                        row
                    )
                )
        );
    }

    isSelected(
        row:
            StockWorkspaceRow
    ): boolean {

        return this.selectedIds.has(
            this.rowId(
                row
            )
        );
    }

    /* =========================================================
       BULK ACTIONS
    ========================================================= */

    bulkDelete() {

        const ids =

            this.selectableRows()

                .filter(
                    row =>
                        this.selectedIds.has(
                            this.rowId(
                                row
                            )
                        )
                )

                .map(
                    row =>
                        row.productId
                )

                .filter(
                    Boolean
                )

                .filter((
                    id,
                    index,
                    self
                ) =>
                    self.indexOf(id)
                    === index
                );

        if (
            !ids.length
        ) {
            return;
        }

        this.productService
            .bulkSoftDelete(
                ids
            )
            .subscribe(() => {

                this.clearSelection();

                this.reload();
            });
    }

    /* =========================================================
       ROW HELPERS
    ========================================================= */

    rowId(
        row:
            StockWorkspaceRow
    ) {

        return row.id;
    }

    rowDeleted(
        row:
            StockWorkspaceRow
    ): boolean {

        return !!row.deleted;
    }

    trackById(
        index: number,
        row:
            StockWorkspaceRow
    ) {

        return row.id;
    }

    value(
        row:
            StockWorkspaceRow,

        column:
            StockWorkspaceColumn<StockWorkspaceRow>
    ) {

        return column.value(
            row
        );
    }

    selectableRows() {

        return this.rows.filter(
            x =>
                x.type
                === 'PRODUCT'
        );
    }

    /* =========================================================
       ACTIONS
    ========================================================= */

    inspect(
        row:
            StockWorkspaceRow
    ) {

        this.router.navigate([
            '/app/stock',
            row.productId
        ]);
    }

    editProduct(
        row: StockWorkspaceRow
    ) {

        this.router.navigate([
            '/app/stock',
            row.productId,
            'edit'
        ]);
    }

    deleteProduct(
        row:
            StockWorkspaceRow
    ) {

        this.productService
            .softDelete(
                row.productId
            )
            .subscribe(() => {

                this.reload();
            });
    }

    restoreProduct(
        row:
            StockWorkspaceRow
    ) {

        this.productService
            .restore(
                row.productId
            )
            .subscribe(() => {

                this.reload();
            });
    }

    createSale(
        row:
            StockWorkspaceRow
    ) {

        this.router.navigate(
            [
                '/app/sales/new'
            ],
            {
                state: {
                    inventorySeed: {

                        productId:
                            row.productId,

                        productName:
                            row.productName,

                        variantId:
                            row.variantId,

                        branchId:
                            row.branchId
                    }
                }
            }
        );
    }

    openReceiveNewProduct() {

        const ref =
            this.dialog.open(
                ProductSelectorDialogComponent,
                {
                    width: '90vw',
                    maxWidth: '1000px',
                    height: '85vh',
                    panelClass:
                        'responsive-dialog'
                }
            );

        ref.afterClosed()
            .subscribe(
                (
                    products: Product[]
                ) => {

                    if (
                        !products ||
                        !products.length
                    ) {
                        return;
                    }

                    this.dialog.open(
                        ReceiveNewProductDialogComponent,
                        {
                            width: '600px',

                            data:
                                products
                        }
                    )
                        .afterClosed()
                        .subscribe(success => {

                            if (success) {
                                this.reload();
                            }
                        });
                }
            );
    }

    openBulkReceive() {

        this.dialog.open(
            InventoryBulkImportDialogComponent,
            {
                width: '1100px',
                maxWidth: '95vw',
                maxHeight: '90vh',
                autoFocus: false
            }
        )
            .afterClosed()
            .subscribe(success => {

                if (success === true) {
                    this.reload();
                }
            });
    }

    openTransactions(
        row: StockWorkspaceRow
    ) {
        const missing: string[] = [];

        if (!row.variantId) {
            missing.push('variant');
        }

        if (!row.branchId) {
            missing.push('branch');
        }

        if (missing.length) {
            this.snackBar.open(
                `Cannot open transactions. Missing ${missing.join(' and ')} information.`,
                'OK',
                {
                    duration: 5000
                }
            );
            return;
        }

        this.router.navigate(
            [
                '/app/stock/inventory',
                row.variantId
            ],
            {
                state: {
                    branchId:
                        row.branchId
                }
            }
        );
    }

    toggleProductStatus(
        row: StockWorkspaceRow
    ) {
        if (row.deleted) {
            this.restoreProduct(row);
        } else {
            this.deleteProduct(row);
        }
    }

    viewVariant(
        row: StockWorkspaceRow
    ) {
        if (!row.variantId) {

            this.snackBar.open(
                'Variant information is missing.',
                'Close',
                {
                    duration: 3000
                }
            );

            return;
        }

        this.variantService
            .getById(row.variantId)
            .subscribe({
                next: variant => {

                    this.dialog.open(
                        VariantDetailsDialogComponent,
                        {
                            panelClass: 'enterprise-dialog',

                            width: 'min(1100px, 94vw)',
                            maxWidth: '94vw',

                            height: 'auto',
                            maxHeight: '92vh',

                            autoFocus: false,
                            restoreFocus: false,

                            data: {
                                variant,
                                branchId: row.branchId
                            }
                        }
                    );
                },
                error: () => {

                    this.snackBar.open(
                        'Failed to load variant.',
                        'Close',
                        {
                            duration: 3000
                        }
                    );
                }
            });
    }

    editVariant(
        row: StockWorkspaceRow
    ) {

        if (!row.variantId) {

            this.snackBar.open(
                'Variant information is missing.',
                'Close',
                {
                    duration: 3000
                }
            );

            return;
        }

        this.variantService
            .getById(row.variantId)
            .subscribe({

                next: variant => {

                    if (!variant) {
                        this.snackBar.open(
                            'Variant was not returned from API.',
                            'Close',
                            { duration: 5000 }
                        );
                        return;
                    }

                    const ref =
                        this.dialog.open(
                            VariantFormComponent,
                            {
                                width: '420px',
                                data: variant
                            }
                        );

                    ref.afterClosed()
                        .subscribe(updated => {

                            if (!updated) {
                                return;
                            }

                            this.snackBar.open(
                                'Variant updated',
                                'Close',
                                {
                                    duration: 2000
                                }
                            );

                            this.reload();
                        });
                },

                error: () => {

                    this.snackBar.open(
                        'Failed to load variant.',
                        'Close',
                        {
                            duration: 3000
                        }
                    );
                }
            });
    }

    manageVariantImages(
        row: StockWorkspaceRow
    ) {

        if (!row.variantId) {

            this.snackBar.open(
                'Variant information is missing.',
                'Close',
                {
                    duration: 3000
                }
            );

            return;
        }

        this.dialog.open(
            VariantImagesDialogComponent,
            {
                panelClass: 'enterprise-dialog',
                width: '1000px',
                maxWidth: '95vw',
                data: {
                    variantId:
                        row.variantId,
                    variantName:
                        row.variantName
                }
            }
        );
    }

    showVariantBarcode(
        row: StockWorkspaceRow
    ) {

        if (!row.variantId) {

            this.snackBar.open(
                'Variant information is missing.',
                'Close',
                {
                    duration: 3000
                }
            );

            return;
        }

        this.dialog.open(
            VariantBarcodeDialogComponent,
            {
                width: '700px',
                maxWidth: '95vw',
                data: {
                    variantId:
                        row.variantId,
                    variantName:
                        row.variantName
                }
            }
        );
    }

    deleteVariant(
        row: StockWorkspaceRow
    ) {

        if (!row.variantId) {

            this.snackBar.open(
                'Variant information is missing.',
                'Close',
                {
                    duration: 3000
                }
            );

            return;
        }

        const ok = confirm(
            `Delete variant "${row.variantName}"?`
        );

        if (!ok) {
            return;
        }

        this.variantService
            .remove(
                row.variantId
            )
            .subscribe({

                next: () => {

                    this.snackBar.open(
                        'Variant deleted',
                        'Close',
                        {
                            duration: 2000
                        }
                    );

                    this.reload();
                },

                error: err => {

                    const msg =
                        err?.error
                        || 'Delete failed';

                    this.snackBar.open(
                        msg,
                        'Close',
                        {
                            duration: 4000
                        }
                    );
                }
            });
    }

    createSaleFromInventory(
        row: StockWorkspaceRow
    ) {
        this.createSale(row);
    }

    openReceiveDialog(
        row: StockWorkspaceRow
    ) {
        this.receive(row);
    }

    openTransferDialog(
        row: StockWorkspaceRow
    ) {
        this.transfer(row);
    }

    openAdjustDialog(
        row: StockWorkspaceRow
    ) {
        this.adjust(row);
    }

    receive(
        row: StockWorkspaceRow
    ) {

        const ref =
            this.dialog.open(
                ReceiveStockDialogComponent,
                {
                    width: '720px',
                    maxWidth: '95vw',

                    data: {
                        productId:
                            row.productId,

                        productName:
                            row.productName,

                        productVariantId:
                            row.variantId,

                        classification:
                            row.variantName,

                        branchId:
                            row.branchId,

                        branchName:
                            row.branchName
                    }
                }
            );

        ref.afterClosed()
            .subscribe(success => {

                if (success) {
                    this.reload();
                }
            });
    }

    transfer(
        row: StockWorkspaceRow
    ) {

        const ref =
            this.dialog.open(
                TransferStockDialogComponent,
                {
                    width: '720px',
                    maxWidth: '95vw',

                    disableClose: false,

                    data: {
                        productVariantId:
                            row.variantId,

                        productName:
                            row.productName,

                        classification:
                            row.variantName,

                        fromBranchId:
                            row.branchId,

                        fromBranchName:
                            row.branchName,

                        available:
                            row.quantityAvailable,

                        averageCost:
                            row.averageCost
                    }
                }
            );

        ref.afterClosed()
            .subscribe(success => {

                if (success) {
                    this.reload();
                }
            });
    }

    adjust(
        row: StockWorkspaceRow
    ) {

        const ref =
            this.dialog.open(
                AdjustStockDialogComponent,
                {
                    width: '560px',
                    maxWidth: '95vw',

                    disableClose: false,

                    data: {
                        productVariantId:
                            row.variantId,

                        productName:
                            row.productName,

                        classification:
                            row.variantName,

                        branchId:
                            row.branchId,

                        branchName:
                            row.branchName
                    }
                }
            );

        ref.afterClosed()
            .subscribe(success => {

                if (success) {
                    this.reload();
                }
            });
    }

    /* =========================================================
       LOADERS
    ========================================================= */

    private loadSupportingData() {

        this.branchService
            .getAllLegacy()
            .subscribe({

                next: x => {

                    this.branches = x;

                    if (x.length === 1) {

                        this.branchId$.next(
                            x[0].id
                        );
                    }

                    this.workspace
                        .setBranches(

                            x.map(
                                branch => ({
                                    id:
                                        branch.id,

                                    name:
                                        branch.name
                                })
                            )
                        );
                }
            });

        this.categoryService
            .getAll(
                'flat',
                false
            )
            .subscribe({

                next: x => {

                    this.categories = x;

                    if (x.length === 1) {

                        this.categoryId$.next(
                            x[0].id
                        );
                    }
                }
            });

        this.supplierService
            .getAll(false)
            .subscribe({

                next: x => {

                    this.suppliers = x;

                    if (x.length === 1) {

                        this.supplierId$.next(
                            x[0].id
                        );
                    }
                }
            });
    }

    private reload() {

        this.page$.next(
            this.page$.value
        );
    }

    /* =========================================================
       THUMBNAILS
    ========================================================= */

    private loadThumbnails() {

        const productIds =
            new Set<string>();

        for (
            const row
            of this.rows
        ) {

            if (
                !row.productId
            ) {
                continue;
            }

            productIds.add(
                row.productId
            );
        }

        for (
            const productId
            of productIds
        ) {

            if (
                this.thumbnails.has(
                    productId
                )
            ) {
                continue;
            }

            this.productService
                .getThumbnailBlob(
                    productId,
                    this.branchServiceBranchId()
                )
                .subscribe({

                    next: blob => {

                        const url =
                            URL.createObjectURL(
                                blob
                            );

                        this.thumbnails.set(

                            productId,

                            this.sanitizer
                                .bypassSecurityTrustUrl(
                                    url
                                )
                        );
                    }
                });
        }
    }

    private branchServiceBranchId():
        string {

        return (
            this.branchId$.value
            ?? this.branches[0]?.id
            ?? ''
        );
    }

    thumbnail(
        productId: string
    ): SafeUrl | null {

        return (

            this.thumbnails.get(
                productId
            )

            ?? null
        );
    }

    openThumbnail(
        productId: string
    ) {

        const thumbnail =
            this.thumbnails.get(
                productId
            );

        if (
            !thumbnail
        ) {
            return;
        }

        this.dialog.open(
            FileViewerDialog,
            {

                panelClass:
                    'file-viewer-dialog',

                maxWidth:
                    '95vw',

                maxHeight:
                    '95vh',

                data: {
                    preview: {

                        src:
                            thumbnail as string,

                        name:
                            'Product Thumbnail',

                        type:
                            'image'
                    }
                }
            }
        );
    }
}