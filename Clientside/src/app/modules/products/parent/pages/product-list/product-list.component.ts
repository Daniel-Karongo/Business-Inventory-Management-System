import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { MatTableModule } from '@angular/material/table';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatPaginator, MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { ViewChild } from '@angular/core';

import { ProductService } from '../../services/product.service';
import { Product } from '../../models/product.model';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatTooltipModule } from '@angular/material/tooltip';
import { ProductBulkImportDialogComponent } from '../../components/product-bulk-import-dialog/product-bulk-import-dialog.component';
import { MatDialog } from '@angular/material/dialog';
import { BreakpointObserver } from '@angular/cdk/layout';
import { Router } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { MatInputModule } from '@angular/material/input';
import { MatFormFieldModule } from '@angular/material/form-field';
import { PageShellComponent } from '../../../../../shared/layout/page-shell/page-shell.component';
import { environment } from '../../../../../../environments/environment';
import { FileViewerDialog } from '../../../../../shared/components/file-viewer/file-viewer.component';
import { ReasonDialogComponent } from '../../../../../shared/components/reason-dialog/reason-dialog.component';
import { CategoryService } from '../../../../categories/services/category.service';
import { MatSelectModule } from '@angular/material/select';
import { BehaviorSubject, combineLatest, debounceTime, switchMap, map, tap, distinctUntilChanged } from 'rxjs';
import { AuthService } from '../../../../auth/services/auth.service';
import { SupplierMinimalDTO } from '../../../../suppliers/models/supplier.model';
import { SupplierService } from '../../../../suppliers/services/supplier.service';

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

  constructor(
    private productService: ProductService,
    private categoryService: CategoryService,
    private supplierService: SupplierService,
    private authService: AuthService,
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
      this.density = this.isMobile ? 'comfortable' : 'compact';
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
      this.search$.pipe(debounceTime(800), distinctUntilChanged()),
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

    const selectedProducts = this.products
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

  bulkSoftDelete() {

    if (this.bulkState !== 'active') {
      this.snackbar.open(
        'Bulk delete failed — selected products must all be active',
        'Close',
        { duration: 2500 }
      );
      return;
    }

    const ref = this.dialog.open(ReasonDialogComponent, {
      width: '480px',
      data: {
        title: 'Confirm Soft Delete',
        message: `You are about to disable ${this.selectedIds.size} product(s).`,
        action: 'DISABLE',
        confirmText: 'Disable',
        cancelText: 'Cancel'
      }
    });

    ref.afterClosed().subscribe(result => {

      if (!result?.confirmed) return;

      const ids = Array.from(this.selectedIds);

      this.productService.bulkSoftDelete(ids, result.reason).subscribe({
        next: () => {
          this.selectedIds.clear(); // ✅ clear selection
          this.snackbar.open(
            `${ids.length} product(s) disabled successfully`,
            'Close',
            { duration: 2000 }
          );
          this.refresh$.next();
        },
        error: () => {
          this.snackbar.open(
            'Bulk soft delete failed',
            'Close',
            { duration: 3000 }
          );
        }
      });

    });
  }

  bulkRestore() {

    if (this.bulkState !== 'deleted') {
      this.snackbar.open(
        'Bulk restore failed — selected products must all be deleted',
        'Close',
        { duration: 2500 }
      );
      return;
    }

    const ref = this.dialog.open(ReasonDialogComponent, {
      width: '480px',
      data: {
        title: 'Confirm Restore',
        message: `You are about to restore ${this.selectedIds.size} product(s).`,
        action: 'RESTORE',
        confirmText: 'Restore',
        cancelText: 'Cancel'
      }
    });

    ref.afterClosed().subscribe(result => {

      if (!result?.confirmed) return;

      const ids = Array.from(this.selectedIds);

      this.productService.bulkRestore(ids, result.reason).subscribe({
        next: () => {
          this.selectedIds.clear(); // ✅ clear selection
          this.snackbar.open(
            `${ids.length} product(s) restored successfully`,
            'Close',
            { duration: 2000 }
          );
          this.refresh$.next();
        },
        error: () => {
          this.snackbar.open(
            'Bulk restore failed',
            'Close',
            { duration: 3000 }
          );
        }
      });

    });
  }

  bulkHardDelete() {

    if (this.bulkState !== 'deleted') {
      this.snackbar.open(
        'Only deleted products can be permanently removed',
        'Close',
        { duration: 2500 }
      );
      return;
    }

    const ref = this.dialog.open(ReasonDialogComponent, {
      width: '480px',
      data: {
        title: 'Confirm Permanent Deletion',
        message: `You are about to permanently delete ${this.selectedIds.size} product(s). This action cannot be undone.`,
        action: 'DELETE',
        confirmText: 'Delete',
        cancelText: 'Cancel'
      }
    });

    ref.afterClosed().subscribe(result => {

      if (!result?.confirmed) return;

      const ids = Array.from(this.selectedIds);

      this.productService.bulkHardDelete(ids, result.reason).subscribe({
        next: () => {
          this.selectedIds.clear();
          this.snackbar.open('Products permanently deleted', 'Close', { duration: 2000 });
          this.refresh$.next();
        },
        error: () => {
          this.snackbar.open('Bulk hard delete failed', 'Close', { duration: 3000 });
        }
      });

    });
  }

  openBulkImport() {
    this.dialog.open(ProductBulkImportDialogComponent, {
      width: '1100px',
      maxWidth: '95vw',
      maxHeight: '90vh',
      autoFocus: false
    }).afterClosed().subscribe(result => {
      if (result === true) {
        this.refresh$.next();
      }
    });
  }
}