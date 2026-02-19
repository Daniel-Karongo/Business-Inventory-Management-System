import { CommonModule } from '@angular/common';
import { Component, OnInit, ViewChild } from '@angular/core';
import { MatButtonModule } from '@angular/material/button';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatIconModule } from '@angular/material/icon';
import { MatPaginator, MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatTableModule } from '@angular/material/table';
import { RouterModule } from '@angular/router';

import { BreakpointObserver } from '@angular/cdk/layout';
import { FormsModule } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatTooltipModule } from '@angular/material/tooltip';
import { Router } from '@angular/router';
import { BehaviorSubject, combineLatest, debounceTime, distinctUntilChanged, of, switchMap, tap } from 'rxjs';
import { environment } from '../../../../../../environments/environment';
import { FileViewerDialog } from '../../../../../shared/components/file-viewer/file-viewer.component';
import { PageShellComponent } from '../../../../../shared/layout/page-shell/page-shell.component';
import { EntityActionConfig, EntityActionService } from '../../../../../shared/services/entity-action.service';
import { AuthService } from '../../../../auth/services/auth.service';
import { CategoryService } from '../../../../categories/services/category.service';
import { SupplierMinimalDTO } from '../../../../suppliers/models/supplier.model';
import { SupplierService } from '../../../../suppliers/services/supplier.service';
import { ProductBulkImportDialogComponent } from '../../components/product-bulk-import-dialog/product-bulk-import-dialog.component';
import {
  PRODUCT_ACTION_REASONS,
  ProductActionType
} from '../../models/product-action-reasons.model';
import { Product } from '../../models/product.model';
import { ProductService } from '../../services/product.service';
import { ProductRestoreOptionsDialog } from '../../components/product-restore-options/product-restore-options.dialog';

@Component({
  selector: 'app-product-list',
  standalone: true,
  imports: [
    CommonModule,
    RouterModule,
    PageShellComponent,
    MatTableModule,
    MatCheckboxModule,
    MatButtonModule,
    MatIconModule,
    MatPaginatorModule,
    MatTooltipModule,
    MatFormFieldModule,
    FormsModule,
    MatInputModule,
    MatSelectModule
  ],
  templateUrl: './product-list.component.html',
  styleUrls: ['./product-list.component.scss']
})
export class ProductListComponent implements OnInit {

  private page$ = new BehaviorSubject<number>(0);
  private serverSize$ = new BehaviorSubject<number>(100);
  private uiPage$ = new BehaviorSubject<number>(0);
  private uiPageSize$ = new BehaviorSubject<number>(20);
  private loadedServerPages = new Set<number>();

  private search$ = new BehaviorSubject<string>('');
  private sortField$ = new BehaviorSubject<string | null>(null);
  private sortDir$ = new BehaviorSubject<'asc' | 'desc'>('asc');
  private statusFilter$ = new BehaviorSubject<'all' | 'active' | 'deleted'>('all');
  private categoryFilter$ = new BehaviorSubject<number | null>(null);
  private refresh$ = new BehaviorSubject<void>(undefined);
  private showDeleted$ = new BehaviorSubject<boolean>(false);

  private minSuppliers$ = new BehaviorSubject<number | null>(null);
  private maxSuppliers$ = new BehaviorSubject<number | null>(null);
  private supplierFilter$ = new BehaviorSubject<string | null>(null);

  allProducts: Product[] = [];

  filteredProducts: Product[] = [];
  products: Product[] = [];
  categories: any[] = [];
  suppliers: SupplierMinimalDTO[] = [];

  loading = true;
  showDeleted = false;
  filters = {
    name: '',
    categoryIds: [],
    includeDeleted: false
  };
  selectedIds = new Set<string>();

  sortField: string | null = null;
  sortDir: 'asc' | 'desc' = 'asc';

  displayedColumns = [
    'select',
    'name',
    'category',
    'variants',
    'sku',
    'suppliers',
    'status',
    'updated',
    'actions'
  ];

  total = 0;
  totalServerElements = 0;
  page = 0;

  private readonly STORAGE_KEY = 'product_list_prefs';
  viewMode: 'table' | 'grid' = 'table';
  density: 'compact' | 'comfortable' = 'compact';
  isMobile = false;

  canManageDeleted = false;

  @ViewChild(MatPaginator) paginator?: MatPaginator;

  private productActionConfig: EntityActionConfig<Product> = {
    entityName: 'Product',
    displayName: (p) => p.name,

    disableReasons: PRODUCT_ACTION_REASONS[ProductActionType.DISABLE],
    restoreReasons: PRODUCT_ACTION_REASONS[ProductActionType.RESTORE],
    deleteReasons: PRODUCT_ACTION_REASONS[ProductActionType.DELETE],

    /* ================= SINGLE ================= */

    disable: (id, reason) =>
      this.productService.softDelete(id, reason),

    restore: (id, reason) =>
      this.openRestoreOptionsDialog().pipe(
        switchMap(options => {
          if (!options) return of(null);
          return this.productService.restore(id, reason, options);
        })
      ),

    hardDelete: (id, reason) =>
      this.productService.hardDelete(id, reason),

    /* ================= BULK ================= */

    disableBulk: (ids, reason) =>
      this.productService.bulkSoftDelete(ids, reason),

    restoreBulk: (ids, reason) =>
      this.openRestoreOptionsDialog().pipe(
        switchMap(options => {
          if (!options) return of(null);
          return this.productService.bulkRestore(ids, reason, options);
        })
      ),

    hardDeleteBulk: (ids, reason) =>
      this.productService.bulkHardDelete(ids, reason),

    /* ================= RELOAD ================= */

    reload: () => {
      this.resetAccumulation();
      this.page$.next(0);
      this.refresh$.next();
      this.clearSelection();
    }
  };

  constructor(
    private productService: ProductService,
    private categoryService: CategoryService,
    private supplierService: SupplierService,
    private authService: AuthService,
    private entityAction: EntityActionService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog,
    private breakpoint: BreakpointObserver,
    private router: Router
  ) { }

  ngOnInit(): void {
    this.loading = true;

    const user = this.authService.getSnapshot();

    if (user) {
      this.canManageDeleted = ['SUPERUSER', 'ADMIN', 'MANAGER'].includes(user.role);
    }

    this.breakpoint.observe(['(max-width: 640px)']).subscribe(res => {
      this.isMobile = res.matches;
      this.viewMode = this.isMobile ? 'grid' : 'table';
      this.density = this.isMobile ? 'compact' : this.density;
    });

    this.loadPreferences();
    this.loadCategories();

    this.supplierService.getAll(false).subscribe({
      next: res => {
        this.suppliers = res.map(s => ({
          id: s.id,
          name: s.name
        }));
      }
    });

    const serverData$ = combineLatest([
      this.page$,
      this.serverSize$,
      this.sortField$,
      this.sortDir$,
      this.search$.pipe(debounceTime(1000), distinctUntilChanged()),
      this.statusFilter$,
      this.categoryFilter$,
      this.showDeleted$,
      this.refresh$,
      this.minSuppliers$,
      this.maxSuppliers$,
      this.supplierFilter$
    ]).pipe(
      tap(() => this.loading = true),
      switchMap(([page, size, sortField, sortDir, search, status, category, showDeleted, refresh, minSuppliers, maxSuppliers, supplierId]) => {

        const params: any = {
          page,
          size,
          includeDeleted: showDeleted
        };

        if (search?.trim()) {
          params.keyword = search.trim();
        }

        if (status === 'active') {
          params.deleted = false;
        }

        if (status === 'deleted') {
          params.deleted = true;
        }

        if (category) {
          params.categoryId = category;
        }

        if (sortField) {
          params.sortBy = sortField;
          params.direction = sortDir;
        }

        if (minSuppliers !== null) {
          params.minSuppliers = minSuppliers;
        }

        if (maxSuppliers !== null) {
          params.maxSuppliers = maxSuppliers;
        }

        if (supplierId) {
          params.supplierId = supplierId;
        }

        return this.productService.getAdvanced(params);
      }),
      tap(res => {

        this.totalServerElements = res.totalElements ?? 0;

        const currentServerPage = this.page$.value;

        // Prevent duplicate append
        if (!this.loadedServerPages.has(currentServerPage) && res.content?.length) {

          const newBatch = (res.content ?? []).map((p: Product) => ({
            ...p,
            thumbnail: `${environment.apiUrl}/products/${p.id}/thumbnail`
          }));

          this.allProducts = [
            ...this.allProducts,
            ...newBatch
          ];

          this.loadedServerPages.add(currentServerPage);
        }

        this.applyClientPagination();
        this.loading = false;
      })
    );

    serverData$.subscribe();
  }

  private applyClientPagination() {

    const page = this.uiPage$.value;
    const size = this.uiPageSize$.value;

    const start = page * size;
    const end = start + size;

    this.products = this.allProducts.slice(start, end);
  }

  private getSelectedProducts(): Product[] {
    return this.allProducts.filter(p => this.selectedIds.has(p.id));
  }

  private resetAccumulation() {
    this.allProducts = [];
    this.loadedServerPages.clear();
    this.uiPage$.next(0);
  }

  loadCategories() {
    this.categoryService.getAll("flat", false).subscribe({
      next: cats => this.categories = cats,
      error: () => { }
    });
  }

  onMinSuppliersChange(value: string) {
    const parsed = value ? +value : null;
    this.setSupplierRange(parsed, this.maxSuppliers$.value);
  }

  onMaxSuppliersChange(value: string) {
    const parsed = value ? +value : null;
    this.setSupplierRange(this.minSuppliers$.value, parsed);
  }

  private setSupplierRange(min: number | null, max: number | null) {

    // Reset if both null
    if (min === null && max === null) {
      this.minSuppliers$.next(null);
      this.maxSuppliers$.next(null);
      return;
    }

    // Prevent invalid range
    if (min !== null && max !== null && min > max) {

      this.snackbar.open(
        'Minimum suppliers cannot be greater than maximum suppliers',
        'Close',
        { duration: 3000 }
      );

      return;
    }

    this.resetAccumulation();
    this.page$.next(0);

    this.minSuppliers$.next(min);
    this.maxSuppliers$.next(max);
  }

  get currentSortField() {
    return this.sortField$.value;
  }

  get currentSortDir() {
    return this.sortDir$.value;
  }

  get currentPageSize() {
    return this.serverSize$.value;
  }

  get currentUiPageSize() {
    return this.uiPageSize$.value;
  }

  getSortIcon(field: string): string {
    if (this.currentSortField !== field) return 'unfold_more';
    return this.currentSortDir === 'asc'
      ? 'arrow_upward'
      : 'arrow_downward';
  }

  onSearchChange(value: string) {
    this.resetAccumulation();
    this.page$.next(0);
    this.search$.next(value);
  }

  onSupplierChange(value: string | null) {
    this.resetAccumulation();
    this.page$.next(0);
    this.supplierFilter$.next(value);
  }

  onStatusChange(value: 'all' | 'active' | 'deleted') {
    this.resetAccumulation();
    this.page$.next(0);
    this.statusFilter$.next(value);
  }

  onCategoryChange(value: number | null) {
    this.resetAccumulation();
    this.page$.next(0);
    this.categoryFilter$.next(value);
  }

  setView(mode: 'grid' | 'table') {

    this.viewMode = mode;

    if (mode === 'table') {
      const newSize = this.density === 'compact' ? 25 : 10;
      this.uiPageSize$.next(newSize);
      this.uiPage$.next(0);
      this.applyClientPagination();
    }

    this.savePreferences();
  }

  toggleDensity() {

    this.density =
      this.density === 'compact'
        ? 'comfortable'
        : 'compact';

    // Adjust UI page size based on density
    const newSize = this.density === 'compact' ? 25 : 10;

    this.uiPageSize$.next(newSize);
    this.uiPage$.next(0);

    this.applyClientPagination();

    this.savePreferences();
  }

  createSale(row: Product) {
    this.router.navigate(['/sales/new'], {
      state: {
        inventorySeed: {
          productId: row.id,
          productName: row.name,
          variantId: null,
          branchId: null
        }
      }
    });
  }

  private savePreferences() {
    localStorage.setItem(this.STORAGE_KEY, JSON.stringify({
      viewMode: this.viewMode,
      density: this.density
    }));
  }

  private loadPreferences() {
    const raw = localStorage.getItem(this.STORAGE_KEY);
    if (!raw) return;

    const prefs = JSON.parse(raw);

    this.viewMode = prefs.viewMode ?? this.viewMode;
    this.density = prefs.density ?? this.density;
  }

  get bulkState(): 'active' | 'deleted' | 'mixed' | null {
    if (!this.selectedIds.size) return null;

    const selectedProducts = this.allProducts
      .filter(p => this.selectedIds.has(p.id));

    const states = new Set(selectedProducts.map(p => p.deleted));
    if (states.size > 1) return 'mixed';
    return states.has(true) ? 'deleted' : 'active';
  }

  sortBy(field: string) {
    this.resetAccumulation();
    this.page$.next(0);

    if (this.sortField$.value === field) {
      this.sortDir$.next(this.sortDir$.value === 'asc' ? 'desc' : 'asc');
    } else {
      this.sortField$.next(field);
      this.sortDir$.next('asc');
    }
  }

  changePage(event: PageEvent) {

    const requestedUiPage = event.pageIndex;
    const pageSize = event.pageSize;

    this.uiPageSize$.next(pageSize);

    const requiredItemIndex = requestedUiPage * pageSize;

    const hasEnoughLocalData =
      requiredItemIndex < this.allProducts.length;

    if (hasEnoughLocalData) {

      this.uiPage$.next(requestedUiPage);
      this.applyClientPagination();
      return;
    }

    // Determine which server page contains this index
    const requiredServerPage =
      Math.floor(requiredItemIndex / this.serverSize$.value);

    const isAlreadyLoaded =
      this.loadedServerPages.has(requiredServerPage);

    if (!isAlreadyLoaded &&
      this.allProducts.length < this.totalServerElements) {

      this.page$.next(requiredServerPage);
    }

    this.uiPage$.next(requestedUiPage);
  }

  onToggleShowDeleted(value: boolean) {
    this.resetAccumulation();
    this.page$.next(0);
    this.showDeleted$.next(value);
  }

  isAllSelected() {
    return this.products.every(p => this.selectedIds.has(p.id));
  }

  masterToggle() {
    const allSelected = this.products.every(p =>
      this.selectedIds.has(p.id)
    );

    if (allSelected) {
      this.products.forEach(p => this.selectedIds.delete(p.id));
    } else {
      this.products.forEach(p => this.selectedIds.add(p.id));
    }
  }

  toggleRowSelection(row: Product) {
    if (this.selectedIds.has(row.id)) {
      this.selectedIds.delete(row.id);
    } else {
      this.selectedIds.add(row.id);
    }
  }

  trackById(index: number, item: Product): string {
    return item.id;
  }

  clearSelection(): void {
    this.selectedIds.clear();
  }

  onThumbnailError(product: Product) {
    product.thumbnail = undefined;
  }

  openImageViewer(product: Product) {
    if (!product.thumbnail) return;

    this.dialog.open(FileViewerDialog, {
      width: '90vw',
      maxWidth: '900px',
      panelClass: 'file-viewer-dialog',
      data: {
        preview: {
          src: product.thumbnail,
          name: product.name,
          type: 'image'
        }
      }
    });
  }

  private openRestoreOptionsDialog() {
    return this.dialog.open(ProductRestoreOptionsDialog, {
      width: '450px',
      disableClose: true
    }).afterClosed();
  }

  toggleProductStatus(p: Product) {
    this.entityAction.toggleSingle(p, this.productActionConfig);
  }

  bulkSoftDelete() {
    const selected = this.getSelectedProducts();
    if (!selected.length) return;

    this.entityAction.bulkDisable(selected, this.productActionConfig);
  }

  bulkRestore() {
    const selected = this.getSelectedProducts();
    if (!selected.length) return;

    this.entityAction.bulkRestore(selected, this.productActionConfig);
  }

  bulkHardDelete() {
    const selected = this.getSelectedProducts();
    if (!selected.length) return;

    this.entityAction.bulkHardDelete(selected, this.productActionConfig);
  }

  openBulkImport() {
    this.dialog.open(ProductBulkImportDialogComponent, {
      width: '1100px',
      maxWidth: '95vw',
      maxHeight: '90vh',
      autoFocus: false
    }).afterClosed().subscribe(result => {
      if (result === true) {
        this.resetAccumulation();
        this.page$.next(0);
        this.refresh$.next();
      }
    });
  }
}