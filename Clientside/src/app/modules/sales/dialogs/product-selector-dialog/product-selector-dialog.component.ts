import { Component, Inject, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { FormControl, ReactiveFormsModule } from '@angular/forms';
import { debounceTime, distinctUntilChanged, switchMap } from 'rxjs';

import { CommonModule } from '@angular/common';
import { MatTableModule } from '@angular/material/table';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { ProductService } from '../../../products/parent/services/product.service';
import { Product } from '../../../products/parent/models/product.model';
import { EntityImageManagerComponent } from '../../../../shared/components/entity-image-manager/entity-image-manager.component';
import { MatDialog } from '@angular/material/dialog';
import { MatTooltipModule } from '@angular/material/tooltip';
import { ProductImageAdapter } from '../../../products/parent/services/product-image.adapter';
import { MatPaginatorModule, PageEvent } from '@angular/material/paginator';

@Component({
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatTableModule,
    MatCheckboxModule,
    MatInputModule,
    MatButtonModule,
    MatIconModule,
    MatTooltipModule,
    MatPaginatorModule,
    MatDialogModule
  ],
  templateUrl: './product-selector-dialog.component.html',
  styleUrls: ['./product-selector-dialog.component.scss']
})
export class ProductSelectorDialogComponent implements OnInit {

  searchCtrl = new FormControl('');
  products: Product[] = [];
  selected = new Set<string>();
  total = 0;
  page = 0;
  size = 20;
  pageSizeOptions = [5, 10, 20, 50];

  sortField: string | null = null;
  sortDir: 'asc' | 'desc' = 'asc';

  displayedColumns = ['select', 'name', 'category', 'sku', 'images'];

  constructor(
    private productService: ProductService,
    private dialogRef: MatDialogRef<ProductSelectorDialogComponent>,
    private dialog: MatDialog, // ✅ FIX
    @Inject(MAT_DIALOG_DATA) public data: {}
  ) { }

  ngOnInit() {
    this.load(); // initial load

    this.searchCtrl.valueChanges.pipe(
      debounceTime(300),
      distinctUntilChanged()
    ).subscribe(() => {
      this.page = 0;       // 🔑 reset paging on new search
      this.load();
    });
  }

  private load() {
    const params: any = {
      page: this.page,
      size: this.size,
      includeDeleted: false
    };

    const q = this.searchCtrl.value?.trim();
    if (q) {
      params.keyword = q; // 🔑 unified search
    }

    if (this.sortField) {
      params.sortBy = this.sortField;
      params.direction = this.sortDir;
    }

    this.productService.getAdvanced(params).subscribe(res => {
      this.products = res.content ?? [];
      this.total = res.totalElements ?? 0;
    });
  }

  sortBy(field: string) {
    if (this.sortField === field) {
      this.sortDir = this.sortDir === 'asc' ? 'desc' : 'asc';
    } else {
      this.sortField = field;
      this.sortDir = 'asc';
    }

    this.load();
  }

  pageChanged(e: PageEvent) {
    this.page = e.pageIndex;
    this.size = e.pageSize;
    this.load();
  }

  toggle(p: Product) {
    this.selected.has(p.id)
      ? this.selected.delete(p.id)
      : this.selected.add(p.id);
  }

  isSelected(p: Product) {
    return this.selected.has(p.id);
  }

  openImages(p: Product) {
    this.dialog.open(EntityImageManagerComponent, {
      width: '900px',
      data: {
        entityId: p.id,
        adapter: ProductImageAdapter(this.productService),
        readonly: true
      }
    });
  }

  confirm() {
    const picked = this.products.filter(p => this.selected.has(p.id));
    this.dialogRef.close(picked);
  }

  cancel() {
    this.dialogRef.close();
  }
}