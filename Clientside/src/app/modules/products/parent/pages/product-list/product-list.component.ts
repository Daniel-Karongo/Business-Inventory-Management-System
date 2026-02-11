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
    MatInputModule
  ],
  templateUrl: './product-list.component.html',
  styleUrls: ['./product-list.component.scss']
})
export class ProductListComponent implements OnInit {

  products: Product[] = [];
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
  page = 0;
  size = 10;

  private readonly STORAGE_KEY = 'product_list_prefs';
  viewMode: 'table' | 'grid' = 'table';
  density: 'compact' | 'comfortable' = 'compact';
  isMobile = false;

  searchTerm = '';
  private searchTimeout: any;

  @ViewChild(MatPaginator) paginator?: MatPaginator;

  constructor(
    private productService: ProductService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog,
    private breakpoint: BreakpointObserver,
    private router: Router
  ) { }

  ngOnInit(): void {
    this.breakpoint.observe(['(max-width: 640px)']).subscribe(res => {
      this.isMobile = res.matches;

      if (this.isMobile) {
        this.viewMode = 'grid';
        this.density = 'comfortable';
      } else {
        this.viewMode = 'table';
        this.density = 'compact';
      }
    });

    this.loadPreferences();
    this.loadProducts();
  }

  loadProducts() {
    this.loading = true;

    const params: any = {
      page: this.page,
      size: this.size,
      includeDeleted: this.showDeleted
    };

    if (this.searchTerm?.trim()) {
      params.keyword = this.searchTerm.trim();
    }

    if (this.sortField) {
      params.sortBy = this.sortField;
      params.direction = this.sortDir;
    }

    this.productService.getAdvanced(params).subscribe({
      next: res => {
        this.products = (res.content || []).map((p: Product) => ({
          ...p,
          thumbnail: `${environment.apiUrl}/products/${p.id}/thumbnail`
        }));

        this.total = res.totalElements || 0;

        this.loading = false;
      },
      error: () => {
        this.loading = false;
      }
    });
  }

  applySearch() {
    this.page = 0;
    this.loadProducts();
  }

  onSearchChange(value: string) {
    clearTimeout(this.searchTimeout);

    this.searchTimeout = setTimeout(() => {
      this.page = 0;
      this.loadProducts();
    }, 300);
  }

  setView(mode: 'grid' | 'table') {
    this.viewMode = mode;

    this.size = this.viewMode === 'grid'
      ? (this.density === 'compact' ? 24 : 12)
      : (this.density === 'compact' ? 25 : 10);

    this.page = 0;
    this.savePreferences();
    this.loadProducts();
  }

  toggleDensity() {
    this.density =
      this.density === 'compact'
        ? 'comfortable'
        : 'compact';

    this.size = this.viewMode === 'grid'
      ? (this.density === 'compact' ? 24 : 12)
      : (this.density === 'compact' ? 25 : 10);

    this.page = 0;
    this.savePreferences();
    this.loadProducts();
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
      density: this.density,
      size: this.size
    }));
  }

  private loadPreferences() {
    const raw = localStorage.getItem(this.STORAGE_KEY);
    if (!raw) return;

    const prefs = JSON.parse(raw);

    this.viewMode = prefs.viewMode ?? this.viewMode;
    this.density = prefs.density ?? this.density;
    this.size = prefs.size ?? this.size;
  }

  get bulkState(): 'active' | 'deleted' | 'mixed' | null {
    if (!this.selectedIds.size) return null;

    const selectedProducts = this.products
      .filter(p => this.selectedIds.has(p.id));

    const states = new Set(selectedProducts.map(p => p.deleted));
    if (states.size > 1) return 'mixed';
    return states.has(true) ? 'deleted' : 'active';
  }

  changePage(event: PageEvent) {
    this.page = event.pageIndex;
    this.size = event.pageSize;
    this.loadProducts();
  }

  sortBy(field: string) {
    if (this.sortField === field) {
      this.sortDir = this.sortDir === 'asc' ? 'desc' : 'asc';
    } else {
      this.sortField = field;
      this.sortDir = field === 'deleted' ? 'asc' : 'asc';
    }
    this.loadProducts();
  }

  toggleDeleted(): void {
    this.showDeleted = !this.showDeleted;
    this.loadProducts();
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
        'Bulk delete failed — selected products have mixed or invalid status',
        'Close',
        { duration: 2500 }
      );
      return;
    }

    const ids = Array.from(this.selectedIds);

    this.productService.bulkSoftDelete(ids).subscribe({
      next: () => {
        this.snackbar.open('Products deleted', 'Close', { duration: 2000 });
        this.loadProducts();
      },
      error: () => {
        this.snackbar.open('Bulk delete failed', 'Close', { duration: 3000 });
      }
    });
  }

  bulkRestore() {
    if (this.bulkState !== 'deleted') {
      this.snackbar.open('Cannot restore active products', 'Close', { duration: 2000 });
      return;
    }

    const ids = Array.from(this.selectedIds);

    this.productService.bulkRestore(ids).subscribe({
      next: () => {
        this.snackbar.open('Products restored', 'Close', { duration: 2000 });
        this.loadProducts();
      },
      error: () => {
        this.snackbar.open('Bulk restore failed', 'Close', { duration: 3000 });
      }
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

    const ids = Array.from(this.selectedIds);

    this.productService.bulkHardDelete(ids).subscribe({
      next: () => {
        this.snackbar.open('Products permanently deleted', 'Close', { duration: 2000 });
        this.loadProducts();
      },
      error: () => {
        this.snackbar.open('Bulk hard delete failed', 'Close', { duration: 3000 });
      }
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
        this.loadProducts();
      }
    });
  }
}