import {
    BreakpointObserver
} from '@angular/cdk/layout';

import {
    CommonModule
} from '@angular/common';

import {
    ChangeDetectionStrategy,
    ChangeDetectorRef,
    Component,
    DestroyRef,
    OnInit,
    TemplateRef,
    ViewChild,
    inject,
} from '@angular/core';

import {
    takeUntilDestroyed
} from '@angular/core/rxjs-interop';

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
    catchError,
    combineLatest,
    debounceTime,
    distinctUntilChanged,
    filter,
    finalize,
    of,
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

import { BranchContextService } from '../../../../../../../core/services/branch-context.service';
import { EntityImageManagerComponent } from '../../../../../../../shared/components/entity-image-manager/entity-image-manager.component';
import { ReasonDialogComponent } from '../../../../../../../shared/components/reason-dialog/reason-dialog.component';
import { LazyThumbnailDirective } from '../../../../../../../shared/directives/lazy-thumbnail.directive';
import { ProductSelectorDialogComponent } from '../../../products/components/product-selector-dialog/product-selector-dialog.component';
import { AdjustStockDialogComponent } from '../../../inventory/components/adjust-stock-dialog/adjust-stock-dialog.component';
import { InventoryBulkImportDialogComponent } from '../../../inventory/components/inventory-bulk-import-dialog/inventory-bulk-import-dialog.component';
import { ReceiveNewProductDialogComponent } from '../../../inventory/components/receive-new-product-dialog/receive-new-product-dialog.component';
import { ReceiveStockDialogComponent } from '../../../inventory/components/receive-stock-dialog/receive-stock-dialog.component';
import { TransferStockDialogComponent } from '../../../inventory/components/transfer-stock-dialog/transfer-stock-dialog.component';
import { Product } from '../../../models/product.model';
import { ProductImageAdapter } from '../../../products/parent/services/product-image.adapter';
import { VariantBarcodeDialogComponent } from '../../../products/variant/components/variant-barcode/variant-barcode-dialog.component';
import { VariantDetailsDialogComponent } from '../../../products/variant/components/variant-details/variant-details-dialog.component';
import { VariantFormComponent } from '../../../products/variant/components/variant-form/variant-form.component';
import { ProductVariantImageAdapter } from '../../../products/variant/services/product-variant-image.adapter';
import { ProductVariantService } from '../../../products/variant/services/product-variant.service';
import { ThumbnailCacheService } from '../../services/thumbnails-cache.service';

@Component({
    selector: 'app-stock-list',
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
        MatSnackBarModule,
        LazyThumbnailDirective
    ],
    changeDetection: ChangeDetectionStrategy.OnPush,
    templateUrl:
        './stock-list.component.html',
    styleUrls: [
        './stock-list.component.scss'
    ]
})
export class StockListComponent
    implements OnInit {

    private cdr = inject(ChangeDetectorRef);

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

    private thumbnailCache =
        inject(ThumbnailCacheService);

    private router =
        inject(Router);

    private destroyRef =
        inject(DestroyRef);

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

    private branchContext = inject(BranchContextService);

    /* =========================================================
       UI
    ========================================================= */

    loadingBranches = false;
    loadingWorkspace = false;

    isMobile = false;

    filtersVisible = true;

    viewMode:
        StockWorkspaceViewMode =
        'table';

    density:
        StockWorkspaceDensity =
        'compact';

    private readonly STORAGE_KEY =
        'stock_workspace_preferences';

    selectedBranchId: string | null = null;

    private lastEmptySearchKey = '';

    showThumbnails = this.loadThumbnailPreference();

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
        new BehaviorSubject<string>(
            ''
        );

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

    filterType:
        'ACTIVE' | 'DELETED' | 'ALL' =
        'ACTIVE';

    private filterType$ =
        new BehaviorSubject<
            'ACTIVE' | 'DELETED' | 'ALL'
        >('ACTIVE');

    private refresh$ = new BehaviorSubject<void>(undefined);

    /* =========================================================
       SELECTION
    ========================================================= */

    selectedIds =
        new Set<string>();

    bulkValidationError:
        string | null = null;

    /* =========================================================
       TABLE CONFIG
    ========================================================= */

    columns:
        StockWorkspaceColumn<StockWorkspaceRow>[] = [
            {
                id: 'product',
                label: 'Product',
                sortable: true,
                value: x => x.productName
            },
            {
                id: 'productSku',
                label: 'Product SKU',
                sortable: true,
                value: x => x.productSku
            },
            {
                id: 'variant',
                label: 'Variant',
                sortable: false,
                value: x => x.variantName
            },
            {
                id: 'variantSku',
                label: 'Variant SKU',
                sortable: false,
                value: x => x.sku
            },
            {
                id: 'branch',
                label: 'Branch',
                sortable: false,
                value: x =>
                    x.branchName
            },
            {
                id: 'onHand',
                label: 'On Hand',
                sortable: false,
                value: x =>
                    x.quantityOnHand
            },
            {
                id: 'reserved',
                label: 'Reserved',
                sortable: false,
                value: x =>
                    x.quantityReserved
            },
            {
                id: 'available',
                label: 'Available',
                sortable: false,
                value: x =>
                    x.quantityAvailable
            },
            {
                id: 'batches',
                label: 'Batches',
                sortable: false,
                value: x =>
                    x.batchCount
            },
            {
                id: 'value',
                label: 'Value',
                sortable: false,
                value: x =>
                    x.inventoryValue
            },
            {
                id: 'avgCost',
                label: 'Avg Cost',
                sortable: false,
                value: x =>
                    x.averageCost
            },
            {
                id: 'fifoCost',
                label: 'FIFO Cost',
                sortable: false,
                value: x =>
                    x.projectedNextSaleCost
            },
            {
                id: 'sellingPrice',
                label: 'Selling Price',
                sortable: false,
                value: x => x.sellingPrice
            },
            {
                id: 'margin',
                label: 'Margin',
                sortable: false,
                value: x =>
                    x.projectedMarginPercent
            },
            {
                id: 'updated',
                label: 'Updated',
                sortable: false,
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

        this.loadPreferences();

        this.breakpoint
            .observe([
                '(max-width: 640px)'
            ])
            .pipe(
                takeUntilDestroyed(
                    this.destroyRef
                )
            )
            .subscribe(result => {
                this.isMobile = result.matches;

                this.cdr.markForCheck();
            });

        this.loadSupportingData();

        combineLatest([
            this.page$,
            this.size$,
            this.keyword$.pipe(
                debounceTime(350),
                distinctUntilChanged()
            ),
            this.branchId$,
            this.categoryId$,
            this.supplierId$,
            this.sortField$,
            this.sortDirection$,
            this.filterType$,
            this.refresh$
        ])
            .pipe(

                // Branch is mandatory
                filter(([
                    ,
                    ,
                    ,
                    branchId
                ]) => !!branchId),

                tap(() => {

                    this.loadingWorkspace = true;

                    this.rows = [];
                    this.total = 0;

                }),

                switchMap(([
                    page,
                    size,
                    keyword,
                    branchId,
                    categoryId,
                    supplierId,
                    sortBy,
                    direction,
                    filterType
                ]) => {
                    return this.workspace.search({
                        filter: filterType,
                        page,
                        size,
                        keyword,
                        branchId:
                            branchId!,
                        categoryId:
                            categoryId
                            ?? undefined,
                        supplierId:
                            supplierId
                            ?? undefined,
                        sortBy,
                        direction
                    }).pipe(
                        catchError(err => {
                            const message =
                                err?.error?.message
                                || err?.error
                                || 'Failed to load stock workspace';

                            this.snackBar.open(
                                message,
                                'Close',
                                {
                                    duration: 5000
                                }
                            );

                            return of({
                                content: [],
                                totalElements: 0,
                                page,
                                size
                            });
                        })
                    );
                })
            )
            .pipe(
                takeUntilDestroyed(
                    this.destroyRef
                )
            )
            .subscribe(result => {

                this.rows = result.content;
                this.total = result.totalElements;

                this.loadingWorkspace = false;

                this.cdr.markForCheck();

                if (
                    result.totalElements === 0 &&
                    (
                        !!this.keyword$.value?.trim()
                        || !!this.categoryId$.value
                        || !!this.supplierId$.value
                    )
                ) {

                    this.snackBar.open(
                        this.emptyStateMessage,
                        'Close',
                        {
                            duration: 5000
                        }
                    );
                }
            });
    }

    /* =========================================================
       DISPLAYED COLUMNS
    ========================================================= */

    get loading(): boolean {
        return this.loadingBranches || this.loadingWorkspace;
    }

    get selectedEntityLabel(): string {
        const type = this.selectedType();

        switch (type) {
            case 'PRODUCT':
                return this.selectedIds.size === 1
                    ? 'product selected'
                    : 'products selected';

            case 'VARIANT':
                return this.selectedIds.size === 1
                    ? 'variant selected'
                    : 'variants selected';

            default:
                return 'selected';
        }
    }

    get emptyStateMessage(): string {

        const branchName =
            this.branches.find(
                b => b.id === this.branchId$.value
            )?.name
            ?? 'selected branch';

        const supplierName =
            this.suppliers.find(
                s => s.id === this.supplierId$.value
            )?.name;

        const categoryName =
            this.categories.find(
                c => c.id === this.categoryId$.value
            )?.name;

        const keyword =
            this.keyword$.value?.trim();

        if (keyword) {
            return `No products match "${keyword}" in ${branchName}.`;
        }

        if (supplierName) {
            return `No products found for supplier "${supplierName}" in ${branchName}.`;
        }

        if (categoryName) {
            return `No products found in category "${categoryName}" for ${branchName}.`;
        }

        return `No products currently exist in ${branchName}.`;
    }

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
                ['variant', 'variantSku']
                    .includes(column.id)
                && !hasVariants
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
                    'value',
                    'avgCost',
                    'fifoCost',
                    'sellingPrice',
                    'margin'
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
        return (this.categories ?? []).length > 1;
    }

    get showSupplierFilter(): boolean {
        return (this.suppliers ?? []).length > 1;
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

    toggleToolbar(): void {
        this.filtersVisible = !this.filtersVisible;
    }

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
            .toggleExpanded(row)
            .subscribe(() => {

                this.rows = [
                    ...this.workspace.getVisibleRows()
                ];

                if (row.type === 'PRODUCT') {
                    this.rows = [
                        ...this.workspace.getVisibleRows()
                    ];
                }

                this.cdr.markForCheck();
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

    setFilterType(
        value:
            'ACTIVE'
            | 'DELETED'
            | 'ALL'
    ) {
        this.page$.next(0);
        this.filterType = value;
        this.filterType$.next(
            value
        );
    }

    setSearch(
        value: string
    ) {

        this.page$.next(0);

        this.keyword$.next(
            value
        );
    }

    setBranch(
        value: string
    ) {

        this.selectedBranchId =
            value;

        this.page$.next(0);

        this.loadBranchFilters(
            value
        );

        this.branchId$.next(
            value
        );
    }

    private loadBranchFilters(
        branchId: string
    ): void {

        this.categories = [];
        this.suppliers = [];

        this.categoryId$.next(
            null
        );

        this.supplierId$.next(
            null
        );

        this.categoryService
            .getAll(
                'flat',
                false,
                branchId
            )
            .subscribe({
                next: categories => {

                    this.categories =
                        categories ?? [];

                    if (
                        this.categories.length === 1
                    ) {

                        this.categoryId$.next(
                            this.categories[0].id
                        );
                    }
                },
                error: () => {

                    this.categories = [];
                }
            });

        this.supplierService
            .getAll(
                false,
                branchId
            )
            .subscribe({
                next: suppliers => {

                    this.suppliers =
                        suppliers ?? [];

                    if (
                        this.suppliers.length === 1
                    ) {

                        this.supplierId$.next(
                            this.suppliers[0].id
                        );
                    }
                },
                error: () => {

                    this.suppliers = [];
                }
            });
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
        value: string | null
    ) {
        this.page$.next(0);

        this.supplierId$.next(
            value === 'ALL'
                ? null
                : value
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
        column: StockWorkspaceColumn<StockWorkspaceRow>
    ) {

        if (!column.sortable) {
            return;
        }

        const field =
            column.id === 'productSku'
                ? 'sku'
                : column.id;

        this.sort(field);
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

    get effectiveViewMode(): StockWorkspaceViewMode {
        return this.isMobile
            ? 'grid'
            : this.viewMode;
    }

    setView(
        mode: StockWorkspaceViewMode
    ) {
        this.viewMode = mode;
        this.savePreferences();
    }

    toggleDensity() {
        this.density =
            this.density === 'compact'
                ? 'comfortable'
                : 'compact';

        this.savePreferences();
    }

    private savePreferences(): void {
        localStorage.setItem(
            this.STORAGE_KEY,
            JSON.stringify({
                viewMode: this.viewMode,
                density: this.density,
                showThumbnails: this.showThumbnails
            })
        );
    }

    private loadPreferences(): void {
        const raw =
            localStorage.getItem(
                this.STORAGE_KEY
            );

        if (!raw) {
            return;
        }

        try {
            const prefs = JSON.parse(raw);

            this.viewMode =
                prefs.viewMode ??
                this.viewMode;

            this.density =
                prefs.density ??
                this.density;

            this.showThumbnails =
                prefs.showThumbnails ??
                this.showThumbnails;
        } catch {
            // ignore corrupted preference data
        }
    }

    /* =========================================================
       SELECTION
    ========================================================= */

    toggleSelection(
        row: StockWorkspaceRow
    ) {

        if (
            !this.isSelectable(
                row
            )
        ) {
            return;
        }

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

        this.validateBulkSelection();
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

        this.bulkValidationError =
            null;
    }

    private validateBulkSelection(): void {

        const selectedRows =
            this.rows.filter(
                row =>
                    this.selectedIds.has(
                        this.rowId(
                            row
                        )
                    )
            );

        if (
            selectedRows.length <= 1
        ) {

            this.bulkValidationError =
                null;

            return;
        }

        const first =
            selectedRows[0];

        const mixedTypes =
            selectedRows.some(
                row =>
                    row.type !==
                    first.type
            );

        if (
            mixedTypes
        ) {

            this.bulkValidationError =
                'Bulk actions require all selected rows to be the same type.';

            return;
        }

        const mixedStatuses =
            selectedRows.some(
                row =>
                    !!row.deleted !==
                    !!first.deleted
            );

        if (
            mixedStatuses
        ) {

            this.bulkValidationError =
                'Bulk actions require all selected rows to have the same status.';

            return;
        }

        this.bulkValidationError =
            null;
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

    hasInventory(
        row: StockWorkspaceRow
    ): boolean {

        return row.hasInventory === true;

    }

    inventoryKnownMissing(
        row: StockWorkspaceRow
    ): boolean {

        return row.hasInventory === false;

    }

    /* =========================================================
       BULK ACTIONS
    ========================================================= */

    bulkDelete() {
        if (this.bulkValidationError) {
            this.snackBar.open(
                this.bulkValidationError,
                'Close',
                {
                    duration: 4000
                }
            );
            return;
        }

        const rows = this.getSelectedRows();

        if (!rows.length) {
            return;
        }

        const type = rows[0].type;

        if (type === 'PRODUCT') {
            const ids = rows.map(row => row.productId);

            this.productService
                .bulkSoftDelete(ids)
                .subscribe({
                    next: () => {
                        this.snackBar.open(
                            'Products deleted',
                            'Close',
                            {
                                duration: 3000
                            }
                        );

                        this.clearSelection();
                        this.reload();
                    },
                    error: err => {
                        const message =
                            err?.error?.message
                            || err?.error
                            || 'Bulk delete failed';

                        this.snackBar.open(
                            message,
                            'Close',
                            {
                                duration: 5000
                            }
                        );
                    }
                });

            return;
        }

        const variantIds =
            rows
                .map(row => row.variantId)
                .filter(Boolean) as string[];

        let completed = 0;
        let failed = false;

        for (const id of variantIds) {
            this.variantService
                .remove(id)
                .subscribe({
                    next: () => {
                        completed++;

                        if (
                            completed === variantIds.length
                        ) {
                            this.snackBar.open(
                                'Variants deleted',
                                'Close',
                                {
                                    duration: 3000
                                }
                            );

                            this.clearSelection();
                            this.reload();
                        }
                    },
                    error: err => {
                        if (failed) {
                            return;
                        }

                        failed = true;

                        const message =
                            err?.error?.message
                            || err?.error
                            || 'Bulk delete failed';

                        this.snackBar.open(
                            message,
                            'Close',
                            {
                                duration: 5000
                            }
                        );
                    }
                });
        }
    }

    bulkRestore() {
        if (this.bulkValidationError) {
            this.snackBar.open(
                this.bulkValidationError,
                'Close',
                {
                    duration: 4000
                }
            );
            return;
        }

        const rows = this.getSelectedRows();

        if (!rows.length) {
            return;
        }

        const type = rows[0].type;

        if (type === 'PRODUCT') {
            const ids =
                rows.map(
                    row => row.productId
                );

            this.productService
                .bulkRestore(ids)
                .subscribe({
                    next: () => {
                        this.snackBar.open(
                            'Products restored',
                            'Close',
                            {
                                duration: 3000
                            }
                        );

                        this.clearSelection();
                        this.reload();
                    },
                    error: err => {
                        const message =
                            err?.error?.message
                            || err?.error
                            || 'Restore failed';

                        this.snackBar.open(
                            message,
                            'Close',
                            {
                                duration: 5000
                            }
                        );
                    }
                });

            return;
        }

        const variantIds =
            rows
                .map(row => row.variantId)
                .filter(Boolean) as string[];

        let completed = 0;
        let failed = false;

        for (const id of variantIds) {
            this.variantService
                .restore(id)
                .subscribe({
                    next: () => {
                        completed++;

                        if (
                            completed === variantIds.length
                        ) {
                            this.snackBar.open(
                                'Variants restored',
                                'Close',
                                {
                                    duration: 3000
                                }
                            );

                            this.clearSelection();
                            this.reload();
                        }
                    },
                    error: err => {
                        if (failed) {
                            return;
                        }

                        failed = true;

                        const message =
                            err?.error?.message
                            || err?.error
                            || 'Restore failed';

                        this.snackBar.open(
                            message,
                            'Close',
                            {
                                duration: 5000
                            }
                        );
                    }
                });
        }
    }

    bulkHardDelete() {
        if (this.bulkValidationError) {
            this.snackBar.open(
                this.bulkValidationError,
                'Close',
                {
                    duration: 4000
                }
            );
            return;
        }

        const rows = this.getSelectedRows();

        if (!rows.length) {
            return;
        }

        const hasActive =
            rows.some(
                row => !row.deleted
            );

        if (hasActive) {
            this.snackBar.open(
                'Hard delete is only allowed for deleted items.',
                'Close',
                {
                    duration: 5000
                }
            );
            return;
        }

        const type = rows[0].type;

        if (type === 'PRODUCT') {
            const ids =
                rows.map(
                    row => row.productId
                );

            this.productService
                .bulkHardDelete(ids)
                .subscribe({
                    next: () => {
                        this.snackBar.open(
                            'Products permanently deleted',
                            'Close',
                            {
                                duration: 3000
                            }
                        );

                        this.clearSelection();
                        this.reload();
                    },
                    error: err => {
                        const message =
                            err?.error?.message
                            || err?.error
                            || 'Hard delete failed';

                        this.snackBar.open(
                            message,
                            'Close',
                            {
                                duration: 5000
                            }
                        );
                    }
                });

            return;
        }

        const variantIds =
            rows
                .map(row => row.variantId)
                .filter(Boolean) as string[];

        let completed = 0;
        let failed = false;

        for (const id of variantIds) {
            this.variantService
                .hardDelete(id)
                .subscribe({
                    next: () => {
                        completed++;

                        if (
                            completed === variantIds.length
                        ) {
                            this.snackBar.open(
                                'Variants permanently deleted',
                                'Close',
                                {
                                    duration: 3000
                                }
                            );

                            this.clearSelection();
                            this.reload();
                        }
                    },
                    error: err => {
                        if (failed) {
                            return;
                        }

                        failed = true;

                        const message =
                            err?.error?.message
                            || err?.error
                            || 'Hard delete failed';

                        this.snackBar.open(
                            message,
                            'Close',
                            {
                                duration: 5000
                            }
                        );
                    }
                });
        }
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
                x.type !==
                'INVENTORY'
        );
    }

    isSelectable(
        row: StockWorkspaceRow
    ): boolean {
        return row.type !== 'INVENTORY';
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
        row: StockWorkspaceRow
    ) {

        this.productService
            .softDelete(
                row.productId
            )
            .subscribe({
                next: () => {

                    this.snackBar.open(
                        'Product deleted',
                        'Close',
                        {
                            duration: 3000
                        }
                    );

                    this.reload();
                },
                error: err => {

                    const message =
                        err?.error?.message
                        || err?.error
                        || 'Delete failed';

                    this.snackBar.open(
                        message,
                        'Close',
                        {
                            duration: 5000
                        }
                    );
                }
            });
    }

    restoreProduct(
        row: StockWorkspaceRow
    ) {

        this.productService
            .restore(
                row.productId
            )
            .subscribe({
                next: () => {

                    this.snackBar.open(
                        'Product restored',
                        'Close',
                        {
                            duration: 3000
                        }
                    );

                    this.reload();
                },
                error: err => {

                    const message =
                        err?.error?.message
                        || err?.error
                        || 'Restore failed';

                    this.snackBar.open(
                        message,
                        'Close',
                        {
                            duration: 5000
                        }
                    );
                }
            });
    }

    hardDeleteProduct(
        row: StockWorkspaceRow
    ) {

        this.productService
            .hardDelete(
                row.productId
            )
            .subscribe({
                next: () => {

                    this.snackBar.open(
                        'Product permanently deleted',
                        'Close',
                        {
                            duration: 3000
                        }
                    );

                    this.reload();
                },
                error: err => {

                    const message =
                        err?.error?.message
                        || err?.error
                        || 'Hard delete failed';

                    this.snackBar.open(
                        message,
                        'Close',
                        {
                            duration: 5000
                        }
                    );
                }
            });
    }

    createSale(
        row: StockWorkspaceRow
    ): void {

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
                            row.variantId ?? null,

                        branchId:
                            row.branchId ?? null,

                        rowType:
                            row.type
                    }
                }
            }
        );
    }

    createProduct(): void {
        this.router.navigate([
            '/app/stock/create'
        ]);
    }

    openReceiveNewProduct() {

        const ref =
            this.dialog.open(
                ProductSelectorDialogComponent,
                {
                    width: '90vw',
                    maxWidth: '1000px',
                    height: '85vh',
                    panelClass: 'enterprise-dialog',
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
                            width: '90vw',
                            maxWidth: '1000px',
                            height: '85vh',
                            panelClass: 'enterprise-dialog',
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

    toggleVariantStatus(
        row: StockWorkspaceRow
    ): void {

        if (row.deleted) {
            this.restoreVariant(row);
            return;
        }

        this.deleteVariant(row);
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
                                panelClass: 'enterprise-dialog',
                                width: '700px',
                                maxWidth: '95vw',
                                maxHeight: '90vh',
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
                { duration: 3000 }
            );

            return;
        }

        const ref =
            this.dialog.open(
                EntityImageManagerComponent,
                {
                    panelClass:
                        'enterprise-dialog',
                    width:
                        '1200px',
                    maxWidth:
                        '95vw',
                    maxHeight:
                        '92vh'
                }
            );

        const adapter =
            ProductVariantImageAdapter(
                this.variantService
            );

        ref.componentInstance.entityId =
            row.variantId;

        ref.componentInstance.adapter =
            adapter;

        adapter.onChange = () => {

            this.thumbnailCache
                .removeVariant(
                    row.variantId!
                );

            this.thumbnailCache
                .finishVariantLoad(
                    row.variantId!
                );

            this.reload();

            this.rows = [...this.rows];
            this.cdr.markForCheck();
        };

        adapter.onThumbnailUpdated =
            adapter.onChange;

        ref.componentInstance.allowHardDelete =
            true;
    }

    manageProductImages(
        row: StockWorkspaceRow
    ): void {

        const ref =
            this.dialog.open(
                EntityImageManagerComponent,
                {
                    panelClass:
                        'enterprise-dialog',
                    width: '1200px',
                    maxWidth: '95vw',
                    maxHeight: '92vh'
                }
            );

        ref.componentInstance.entityId =
            row.productId;

        const adapter =
            ProductImageAdapter(
                this.productService
            );

        adapter.onChange = () => {

            this.invalidateProductImageCache(
                row.productId
            );

            this.reload();
        };

        adapter.onThumbnailUpdated = () => {

            this.invalidateProductImageCache(
                row.productId
            );

            this.reload();
        };

        ref.componentInstance.adapter =
            adapter;

        ref.componentInstance.allowHardDelete =
            true;
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
                panelClass: 'enterprise-dialog',
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

        const variantId =
            row.variantId;

        const ref =
            this.dialog.open(
                ReasonDialogComponent,
                {
                    width: '500px',
                    panelClass: 'enterprise-dialog',
                    data: {
                        title: 'Delete Variant',
                        message:
                            `Delete variant "${row.variantName}"?`,
                        action: 'DELETE',
                        requireReason: false,
                        allowCustomReason: true,
                        reasons: [
                            'Duplicate variant',
                            'Obsolete variant',
                            'Incorrect creation',
                            'Discontinued'
                        ]
                    }
                }
            );

        ref.afterClosed()
            .subscribe(result => {

                if (!result?.confirmed) {
                    return;
                }

                this.variantService
                    .remove(
                        variantId,
                        result.reason
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

            });
    }

    restoreVariant(
        row: StockWorkspaceRow
    ) {

        if (!row.variantId) {
            return;
        }

        const variantId =
            row.variantId;

        const ref =
            this.dialog.open(
                ReasonDialogComponent,
                {
                    width: '500px',
                    panelClass: 'enterprise-dialog',
                    data: {
                        title: 'Restore Variant',
                        message: `Restore variant "${row.variantName}"?`,
                        action: 'RESTORE',
                        requireReason: false,
                        allowCustomReason: true
                    }
                }
            );

        ref.afterClosed()
            .subscribe(result => {

                if (!result?.confirmed) {
                    return;
                }

                this.variantService
                    .restore(
                        variantId,
                        result.reason
                    )
                    .subscribe({

                        next: () => {

                            this.snackBar.open(
                                'Variant restored',
                                'Close',
                                {
                                    duration: 2000
                                }
                            );

                            this.reload();
                        }

                    });

            });
    }

    hardDeleteVariant(
        row: StockWorkspaceRow
    ) {

        if (
            !row.variantId
        ) {
            return;
        }

        this.variantService
            .hardDelete(
                row.variantId
            )
            .subscribe({
                next: () => {

                    this.snackBar.open(
                        'Variant permanently deleted',
                        'Close',
                        {
                            duration: 3000
                        }
                    );

                    this.reload();
                },
                error: err => {

                    const message =
                        err?.error?.message
                        || err?.error
                        || 'Hard delete failed';

                    this.snackBar.open(
                        message,
                        'Close',
                        {
                            duration: 5000
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

    receiveInitialStockForVariant(
        row: StockWorkspaceRow
    ): void {
        if (!row.variantId) {
            return;
        }

        const ref =
            this.dialog.open(
                ReceiveStockDialogComponent,
                {
                    width: '720px',
                    maxWidth: '95vw',
                    panelClass: 'enterprise-dialog',
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
                            this.branchId$.value,
                        branchName:
                            this.branches.find(
                                b => b.id === this.branchId$.value
                            )?.name
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

    receive(
        row: StockWorkspaceRow
    ) {

        const ref =
            this.dialog.open(
                ReceiveStockDialogComponent,
                {
                    width: '720px',
                    maxWidth: '95vw',
                    panelClass: 'enterprise-dialog',
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
                    panelClass: 'enterprise-dialog',
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
                    panelClass: 'enterprise-dialog',
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



    private loadSupportingData(): void {

        this.loadingBranches = true;

        this.branchService
            .getAllLegacy()
            .pipe(
                finalize(() => {
                    this.loadingBranches = false;
                    this.cdr.detectChanges();
                })
            )
            .subscribe({

                next: branches => {

                    this.branches =
                        branches ?? [];

                    let selectedBranchId:
                        string | null = null;

                    const currentBranch =
                        this.branchContext.currentBranch;

                    if (currentBranch) {

                        selectedBranchId =
                            currentBranch;

                    } else if (
                        this.branches.length === 1
                    ) {

                        selectedBranchId =
                            this.branches[0].id;

                    }

                    if (selectedBranchId) {

                        this.selectedBranchId =
                            selectedBranchId;

                        /*
                         * Load branch-specific
                         * categories + suppliers
                         * BEFORE triggering search.
                         */
                        this.loadBranchFilters(
                            selectedBranchId
                        );

                        this.branchId$.next(
                            selectedBranchId
                        );

                    }

                    this.workspace
                        .setBranches(
                            this.branches.map(
                                branch => ({
                                    id:
                                        branch.id,
                                    name:
                                        branch.name
                                })
                            )
                        );

                },

                error: err => {

                    this.loadingBranches = false;

                    this.rows = [];
                    this.total = 0;

                    console.error(
                        'Failed to load branches',
                        err
                    );

                    this.cdr.detectChanges();
                }

            });

    }

    private getSelectedRows():
        StockWorkspaceRow[] {

        return this.rows.filter(
            row =>
                this.selectedIds.has(
                    this.rowId(
                        row
                    )
                )
        );
    }

    private selectedType():
        'PRODUCT'
        | 'VARIANT'
        | null {

        const rows =
            this.getSelectedRows();

        if (
            !rows.length
        ) {
            return null;
        }

        return rows[0]
            .type as
            'PRODUCT'
            | 'VARIANT';
    }

    canBulkRestore():
        boolean {

        const rows =
            this.getSelectedRows();

        return (
            rows.length > 0
            &&
            rows.every(
                row =>
                    !!row.deleted
            )
        );
    }

    canBulkHardDelete():
        boolean {

        const rows =
            this.getSelectedRows();

        return (
            rows.length > 0
            &&
            rows.every(
                row =>
                    !!row.deleted
            )
        );
    }

    canBulkSoftDelete():
        boolean {

        const rows =
            this.getSelectedRows();

        return (
            rows.length > 0
            &&
            rows.every(
                row =>
                    !row.deleted
            )
        );
    }

    private reload(): void {
        this.refresh$.next();
    }

    /* =========================================================
       THUMBNAILS
    ========================================================= */

    thumbnailForRow(
        row: StockWorkspaceRow
    ): SafeUrl | null {

        if (
            row.variantId
        ) {

            const variant =
                this.thumbnailCache.peekVariant(
                    row.variantId
                );

            if (variant) {
                return variant;
            }
        }

        return (
            this.thumbnailCache.peekProduct(
                row.productId
            )
            ?? null
        );
    }

    ensureThumbnailLoaded(
        row: StockWorkspaceRow
    ): void {

        if (!this.showThumbnails) {
            return;
        }

        /*
         * Variant thumbnail
         * (actual variant image)
         */
        if (
            row.variantId &&
            row.type === 'VARIANT' &&
            row.thumbnailFileName
        ) {
            this.loadVariantThumbnail(
                row
            );
            return;
        }

        /*
         * Inventory inherits
         * from variant/product
         */
        if (
            row.type === 'INVENTORY'
            &&
            row.thumbnailFileName
        ) {

            if (
                row.variantId
            ) {
                this.loadVariantThumbnail(
                    {
                        ...row,
                        type: 'VARIANT'
                    }
                );
            }

            return;
        }

        /*
         * Product thumbnail
         */
        if (
            row.productId &&
            row.thumbnailFileName
        ) {

            if (
                this.thumbnailCache.peekProduct(
                    row.productId
                )
            ) {
                return;
            }

            if (
                !this.thumbnailCache.beginProductLoad(
                    row.productId
                )
            ) {
                return;
            }

            this.productService
                .getSharedThumbnailBlob(
                    row.thumbnailFileName,
                    this.branchServiceBranchId()
                )
                .pipe(
                    takeUntilDestroyed(
                        this.destroyRef
                    )
                )
                .subscribe({
                    next: blob => {

                        this.thumbnailCache
                            .setProduct(
                                row.productId,
                                URL.createObjectURL(
                                    blob
                                )
                            );

                        this.thumbnailCache
                            .finishProductLoad(
                                row.productId
                            );

                        this.rows = [
                            ...this.rows
                        ];

                        this.cdr.markForCheck();
                    },
                    error: () => {

                        this.thumbnailCache
                            .finishProductLoad(
                                row.productId
                            );
                    }
                });
        }
    }

    private loadVariantThumbnail(
        row: StockWorkspaceRow
    ): void {

        if (
            !row.variantId ||
            !row.thumbnailFileName
        ) {
            return;
        }

        const variantId = row.variantId;

        if (
            this.thumbnailCache.peekVariant(
                row.variantId
            )
        ) {
            return;
        }

        if (
            !this.thumbnailCache
                .beginVariantLoad(
                    variantId
                )
        ) {
            return;
        }

        this.variantService
            .getSharedThumbnailBlob(
                row.thumbnailFileName
            )
            .pipe(
                takeUntilDestroyed(
                    this.destroyRef
                )
            )
            .subscribe({
                next: blob => {

                    this.thumbnailCache
                        .setVariant(
                            variantId,
                            URL.createObjectURL(
                                blob
                            )
                        );

                    this.thumbnailCache
                        .finishVariantLoad(
                            variantId
                        );

                    this.rows = [...this.rows];
                    this.cdr.markForCheck();
                },
                error: () => {

                    this.thumbnailCache
                        .finishVariantLoad(
                            variantId
                        );
                }
            });
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
            this.thumbnailCache
                .getProduct(
                    productId
                )
            ?? null
        );
    }

    private loadThumbnailPreference(): boolean {
        const stored =
            localStorage.getItem(
                'stock.showThumbnails'
            );

        if (stored !== null) {
            return stored === 'true';
        }

        const connection =
            (navigator as any)?.connection;

        const effectiveType =
            connection?.effectiveType;

        if (
            effectiveType === 'slow-2g' ||
            effectiveType === '2g'
        ) {
            return false;
        }

        return true;
    }

    toggleThumbnails(): void {
        this.showThumbnails =
            !this.showThumbnails;

        localStorage.setItem(
            'stock.showThumbnails',
            String(this.showThumbnails)
        );

        this.rows = [...this.rows];
        this.cdr.markForCheck();

        if (this.showThumbnails) {
            queueMicrotask(() => {
                for (const row of this.rows) {
                    this.ensureThumbnailLoaded(row);
                }
            });
        }
    }

    private clearThumbnailCaches(): void {

        this.thumbnailCache.clear();

        this.rows = [...this.rows];
        this.cdr.markForCheck();
    }

    openThumbnail(
        row: StockWorkspaceRow
    ): void {
        if (
            row.variantId &&
            row.primaryImageFileName
        ) {

            this.variantService
                .getImageBlob(
                    row.variantId,
                    row.primaryImageFileName
                )
                .pipe(
                    takeUntilDestroyed(
                        this.destroyRef
                    )
                )
                .subscribe(blob => {

                    this.openImageViewer(
                        blob,
                        row.variantName
                        ?? row.productName
                    );
                });

            return;
        }

        if (
            row.productId &&
            row.primaryImageFileName
        ) {

            this.productService
                .getImageBlob(
                    row.productId,
                    row.primaryImageFileName,
                    this.branchServiceBranchId()
                )
                .pipe(
                    takeUntilDestroyed(
                        this.destroyRef
                    )
                )
                .subscribe(blob => {

                    this.openImageViewer(
                        blob,
                        row.productName
                    );
                });
        }
    }

    private openImageViewer(
        blob: Blob,
        name: string
    ): void {
        const url =
            URL.createObjectURL(blob);

        const dialogRef =
            this.dialog.open(
                FileViewerDialog,
                {
                    panelClass: 'enterprise-dialog',
                    width: '90vw',
                    maxWidth: '90vw',
                    height: '90vh',
                    maxHeight: '90vh',
                    data: {
                        preview: {
                            src: url,
                            name,
                            type: 'image'
                        }
                    }
                }
            );

        dialogRef
            .afterClosed()
            .subscribe(() => {
                URL.revokeObjectURL(
                    url
                );
            });
    }

    public invalidateProductImageCache(
        productId: string
    ): void {

        this.thumbnailCache
            .removeProduct(
                productId
            );

        this.thumbnailCache
            .finishProductLoad(
                productId
            );
    }
}