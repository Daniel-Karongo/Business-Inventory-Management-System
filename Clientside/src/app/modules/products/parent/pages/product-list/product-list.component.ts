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
import { SelectionModel } from '@angular/cdk/collections';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatTooltipModule } from '@angular/material/tooltip';
import { ProductBulkImportDialogComponent } from '../../components/product-bulk-import-dialog/product-bulk-import-dialog.component';
import { MatDialog } from '@angular/material/dialog';

@Component({
  selector: 'app-product-list',
  standalone: true,
  imports: [
    CommonModule,
    RouterModule,
    MatTableModule,
    MatCheckboxModule,
    MatButtonModule,
    MatIconModule,
    MatPaginatorModule,
    MatTooltipModule
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
  selection = new SelectionModel<Product>(true, []);

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

  @ViewChild(MatPaginator) paginator?: MatPaginator;

  constructor(
    private productService: ProductService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {
    this.loadProducts();
  }

  loadProducts() {
    this.loading = true;

    const params: any = {
      page: this.page,
      size: this.size,
      includeDeleted: this.showDeleted
    };

    if (this.sortField) {
      params.sortBy = this.sortField;
      params.direction = this.sortDir;
    }

    this.productService.getAdvanced(params).subscribe({
      next: res => {
        this.products = res.content || [];
        this.total = res.totalElements || 0;

        this.selection.clear(); // reset bulk selection on page change
        this.loading = false;
      },
      error: () => {
        this.loading = false;
      }
    });
  }

  get bulkState(): 'active' | 'deleted' | 'mixed' | null {
    const selected = this.selection.selected;
    if (selected.length === 0) return null;

    const states = new Set(selected.map(p => p.deleted));
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
    const numSelected = this.selection.selected.length;
    const numRows = this.products.length;
    return numSelected === numRows && numRows > 0;
  }

  masterToggle() {
    if (this.isAllSelected()) {
      this.selection.clear();
    } else {
      this.products.forEach(row => this.selection.select(row));
    }
  }

  toggleRowSelection(row: Product) {
    this.selection.toggle(row);
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

    const ids = this.selection.selected.map(p => p.id);

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

    const ids = this.selection.selected.map(p => p.id);

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

    const ids = this.selection.selected.map(p => p.id);

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