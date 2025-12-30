import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatTableModule } from '@angular/material/table';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatSnackBar } from '@angular/material/snack-bar';

import { InventoryService } from '../../services/inventory.service';
import { InventoryResponse } from '../../models/inventory-response.model';
import { BranchService } from '../../../branches/services/branch.service';
import { BranchMinimalDTO } from '../../../branches/models/branch.model';
import { RouterModule } from '@angular/router';
import { MatPaginator, MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { ViewChild } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { AdjustStockDialogComponent } from '../../components/adjust-stock-dialog/adjust-stock-dialog.component';
import { ReceiveStockDialogComponent } from '../../components/receive-stock-dialog/receive-stock-dialog.component';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatSelectChange, MatSelectModule } from '@angular/material/select';

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
    MatFormFieldModule
  ],
  templateUrl: './inventory-list.component.html',
  styleUrls: ['./inventory-list.component.scss']
})
export class InventoryListComponent implements OnInit {

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

  constructor(
    private inventoryService: InventoryService,
    private branchService: BranchService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {
    this.loadBranches();
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
      next: res => {
        this.inventory = res.data || [];
        this.filtered = [...this.inventory];

        this.applySorting();     // 🔥 INSERT
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

  openReceiveDialog(row: InventoryResponse) {
    const ref = this.dialog.open(ReceiveStockDialogComponent, {
      width: '600px',
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
      width: '500px',
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

}