import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatTableModule } from '@angular/material/table';
import { MatTooltipModule } from '@angular/material/tooltip';

import { ViewChild } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatPaginator, MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatSelectChange, MatSelectModule } from '@angular/material/select';
import { Router, RouterModule } from '@angular/router';
import { BranchMinimalDTO } from '../../../branches/models/branch.model';
import { BranchService } from '../../../branches/services/branch.service';
import { Product } from '../../../products/parent/models/product.model';
import { ProductSelectorDialogComponent } from '../../../sales/dialogs/product-selector-dialog/product-selector-dialog.component';
import { AdjustStockDialogComponent } from '../../components/adjust-stock-dialog/adjust-stock-dialog.component';
import { InventoryBulkImportDialogComponent } from '../../components/inventory-bulk-import-dialog/inventory-bulk-import-dialog.component';
import { ReceiveNewProductDialogComponent } from '../../components/receive-new-product-dialog/receive-new-product-dialog.component';
import { ReceiveStockDialogComponent } from '../../components/receive-stock-dialog/receive-stock-dialog.component';
import { TransferStockDialogComponent } from '../../components/transfer-stock-dialog/transfer-stock-dialog.component';
import { InventoryResponse } from '../../models/inventory-response.model';
import { InventoryService } from '../../services/inventory.service';
import { PageShellComponent } from '../../../../shared/layout/page-shell/page-shell.component';
import { BreakpointObserver } from '@angular/cdk/layout';
import { FormsModule } from '@angular/forms';
import { MatInputModule } from '@angular/material/input';

@Component({
  selector: 'app-inventory-list',
  standalone: true,
  imports: [
    CommonModule,
    MatTableModule,
    MatButtonModule,
    MatIconModule,
    MatTooltipModule,
    RouterModule,
    MatPaginatorModule,
    MatSelectModule,
    MatFormFieldModule,
    PageShellComponent,
    FormsModule,
    MatInputModule
  ],
  templateUrl: './inventory-list.component.html',
  styleUrls: ['./inventory-list.component.scss']
})
export class InventoryListComponent implements OnInit {

  viewMode: 'table' | 'grid' = 'table';
  density: 'compact' | 'comfortable' = 'compact';
  isMobile = false;

  loading = true;

  inventory: InventoryResponse[] = [];
  filtered: InventoryResponse[] = [];

  branches: BranchMinimalDTO[] = [];
  selectedBranchId?: string;

  displayedColumns = [
    'product',
    'variant',
    'variantSku',
    'branch',
    'onHand',
    'reserved',
    'available',
    'batches',
    'value',
    'updated',
    'actions'
  ];

  sortField?: 'productName'
    | 'productClassification'
    | 'productVariantSKU'
    | 'branchName'
    | 'quantityOnHand'
    | 'quantityReserved'
    | 'available'
    | 'lastUpdatedAt';

  sortDir: 'asc' | 'desc' = 'asc';

  @ViewChild(MatPaginator) paginator?: MatPaginator;

  page = 0;
  size = 10;
  total = 0;

  pagedData: InventoryResponse[] = [];
  searchTerm = '';
  private readonly STORAGE_KEY = 'inventory_list_prefs';

  constructor(
    private inventoryService: InventoryService,
    private branchService: BranchService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog,
    private breakpoint: BreakpointObserver,
    private router: Router
  ) { }

  ngOnInit(): void {
    this.breakpoint.observe(['(max-width: 640px)']).subscribe(result => {
      this.isMobile = result.matches;

      if (this.isMobile) {
        this.viewMode = 'grid';
        this.density = 'comfortable';
      } else {
        this.viewMode = 'table';
        this.density = 'compact';
      }
    });

    this.loadBranches();
    this.loadPreferences();
    this.loadInventory();
  }

  loadBranches() {
    this.branchService.getAll(false).subscribe({
      next: branches => {
        this.branches = branches
          .filter(b => !!b.id) // hard safety gate
          .map(b => ({
            id: b.id!,          // now guaranteed
            name: b.name,
            branchCode: b.branchCode ?? ''
          }));
      },
      error: () => {
        this.snackbar.open('Failed to load branches', 'Close', { duration: 3000 });
      }
    });
  }

  loadInventory() {
    this.loading = true;

    const req = this.selectedBranchId
      ? this.inventoryService.getByBranch(this.selectedBranchId)
      : this.inventoryService.getAll();

    req.subscribe({
      next: inventory => {
        this.inventory = inventory;
        this.filtered = [...this.inventory];

        this.applySorting();
        this.page = 0;
        this.applyPagination();

        this.loading = false;
      },
      error: () => {
        this.loading = false;
        this.snackbar.open('Failed to load inventory', 'Close', { duration: 3000 });
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

  applySearch() {
    const term = this.searchTerm.trim().toLowerCase();

    if (!term) {
      this.filtered = [...this.inventory];
    } else {
      this.filtered = this.inventory.filter(r =>
        r.productName.toLowerCase().includes(term) ||
        r.productVariantSKU.toLowerCase().includes(term)
      );
    }

    this.applySorting();
    this.page = 0;
    this.applyPagination();
  }

  applySorting() {
    if (!this.sortField) return;

    const field = this.sortField;           // 🔑 snapshot (narrowed)
    const dir = this.sortDir === 'asc' ? 1 : -1;

    this.filtered.sort((a, b) => {
      let av: any;
      let bv: any;

      switch (field) {
        case 'available':
          av = this.available(a);
          bv = this.available(b);
          break;

        case 'lastUpdatedAt':
          av = new Date(a.lastUpdatedAt || 0).getTime();
          bv = new Date(b.lastUpdatedAt || 0).getTime();
          break;

        default:
          av = (a as any)[field];
          bv = (b as any)[field];
      }

      if (av == null) return 1;
      if (bv == null) return -1;

      if (typeof av === 'string') {
        return av.localeCompare(bv) * dir;
      }

      return (av > bv ? 1 : av < bv ? -1 : 0) * dir;
    });
  }

  sortBy(field: typeof this.sortField) {
    if (this.sortField === field) {
      this.sortDir = this.sortDir === 'asc' ? 'desc' : 'asc';
    } else {
      this.sortField = field;
      this.sortDir = 'asc';
    }

    this.applySorting();
    this.applyPagination();
  }

  applyPagination() {
    this.total = this.filtered.length;

    const start = this.page * this.size;
    const end = start + this.size;

    this.pagedData = this.filtered.slice(start, end);
  }

  changePage(event: PageEvent) {
    this.page = event.pageIndex;
    this.size = event.pageSize;
    this.applyPagination();
  }

  onBranchChange(event: MatSelectChange) {
    const value = event.value || undefined;
    this.changeBranch(value);
  }

  changeBranch(branchId?: string) {
    this.selectedBranchId = branchId;
    this.loadInventory();
  }

  available(row: InventoryResponse): number {
    return (row.quantityOnHand || 0) - (row.quantityReserved || 0);
  }

  setView(mode: 'grid' | 'table') {
    this.viewMode = mode;

    this.size = this.viewMode === 'grid'
      ? (this.density === 'compact' ? 25 : 12)
      : (this.density === 'compact' ? 25 : 10);

    this.page = 0;
    this.savePreferences();
    this.applyPagination();
  }

  toggleDensity() {
    this.density =
      this.density === 'compact'
        ? 'comfortable'
        : 'compact';

    this.size = this.viewMode === 'grid'
      ? (this.density === 'compact' ? 25 : 12)
      : (this.density === 'compact' ? 25 : 10);

    this.page = 0;
    this.savePreferences();
    this.applyPagination();
  }

  createSaleFromInventory(row: InventoryResponse) {
    this.router.navigate(['/sales/new'], {
      state: {
        inventorySeed: {
          productId: row.productId,
          productName: row.productName,
          variantId: row.productVariantId,
          branchId: row.branchId
        }
      }
    });
  }

  openReceiveDialog(row: InventoryResponse) {
    const ref = this.dialog.open(ReceiveStockDialogComponent, {
      width: '720px',
      maxWidth: '95vw',
      data: {
        productId: row.productId,
        productName: row.productName,
        productVariantId: row.productVariantId,
        classification: row.productClassification,
        branchId: row.branchId,
        branchName: row.branchName
      }
    });

    ref.afterClosed().subscribe(success => {
      if (success) this.loadInventory();
    });
  }

  openAdjustDialog(row: InventoryResponse) {
    const ref = this.dialog.open(AdjustStockDialogComponent, {
      width: '560px',
      maxWidth: '95vw',
      disableClose: false,
      data: {
        productVariantId: row.productVariantId,
        productName: row.productName,
        classification: row.productClassification,
        branchId: row.branchId,
        branchName: row.branchName
      }
    });

    ref.afterClosed().subscribe(success => {
      if (success) this.loadInventory();
    });
  }

  openTransferDialog(row: InventoryResponse) {
    const ref = this.dialog.open(TransferStockDialogComponent, {
      width: '720px',
      maxWidth: '95vw',
      disableClose: false,
      data: {
        productVariantId: row.productVariantId,
        productName: row.productName,
        classification: row.productClassification,
        fromBranchId: row.branchId,
        fromBranchName: row.branchName,
        available: this.available(row),
        averageCost: row.averageCost
      }
    });

    ref.afterClosed().subscribe(success => {
      if (success) this.loadInventory();
    });
  }

  openReceiveNewProduct() {
    const ref = this.dialog.open(ProductSelectorDialogComponent, {
      width: '90vw',
      maxWidth: '1000px',
      height: '85vh',
      panelClass: 'responsive-dialog'
    });

    ref.afterClosed().subscribe((products: Product[]) => {
      if (!products || !products.length) return;

      this.dialog.open(ReceiveNewProductDialogComponent, {
        width: '600px',
        data: products
      }).afterClosed().subscribe(success => {
        if (success) this.loadInventory();
      });
    });
  }

  openBulkReceive() {
    this.dialog.open(InventoryBulkImportDialogComponent, {
      width: '1100px',
      maxWidth: '95vw',
      maxHeight: '90vh',
      autoFocus: false
    }).afterClosed().subscribe(success => {
      if (success === true) {
        this.loadInventory();
      }
    });
  }
}