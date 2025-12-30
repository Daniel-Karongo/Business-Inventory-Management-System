import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ActivatedRoute } from '@angular/router';
import { MatTableModule } from '@angular/material/table';
import { MatIconModule } from '@angular/material/icon';
import { MatTooltipModule } from '@angular/material/tooltip';

import { InventoryService } from '../../services/inventory.service';
import { StockTransactionDTO } from '../../models/stock-transaction.model';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatSelectModule } from '@angular/material/select';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';
import { FormsModule } from '@angular/forms';
import { MatInputModule } from '@angular/material/input';

@Component({
  standalone: true,
  selector: 'app-inventory-transactions',
  imports: [
    CommonModule,
    MatTableModule,
    MatIconModule,
    MatTooltipModule,
    MatFormFieldModule,
    MatSelectModule,
    MatDatepickerModule,
    MatNativeDateModule,
    FormsModule,
    MatInputModule
  ],
  templateUrl: './inventory-transactions.component.html',
  styleUrls: ['./inventory-transactions.component.scss']
})
export class InventoryTransactionsComponent implements OnInit {

  loading = true;
  transactions: StockTransactionDTO[] = [];
  variantId!: string;
  variantName!: string | undefined;
  productName!: string | undefined;

  displayedColumns = [
    'timestamp',
    'type',
    'quantity',
    'branch',
    'reference',
    'note',
    'performedBy'
  ];

  filtered: StockTransactionDTO[] = [];

  fromDate?: Date;
  toDate?: Date;

  selectedTypes: string[] = [];
  direction: 'IN' | 'OUT' | 'ALL' = 'ALL';

  readonly transactionTypes = [
    'RECEIPT',
    'SALE',
    'ADJUSTMENT',
    'TRANSFER_IN',
    'TRANSFER_OUT',
    'RESERVATION',
    'RELEASE',
    'RETURN'
  ];

  constructor(
    private route: ActivatedRoute,
    private inventoryService: InventoryService
  ) { }

  ngOnInit() {
    this.variantId = this.route.snapshot.paramMap.get('variantId')!;
    this.loadTransactions();
  }

  loadTransactions() {
    this.inventoryService.getTransactionsByVariant(this.variantId).subscribe({
      next: data => {
        this.transactions = data;
        this.variantName = this.transactions.at(0)?.productVariantName;
        this.productName = this.transactions.at(0)?.productName;
        this.applyFilters();
        this.loading = false;

      },
      error: () => {
        this.loading = false;
      }
    });
  }

  applyFilters() {
    this.filtered = this.transactions.filter(t => {

      // Date filter
      const ts = new Date(t.timestamp).getTime();

      if (this.fromDate && ts < this.fromDate.getTime()) return false;
      if (this.toDate && ts > this.toDate.getTime()) return false;

      // Type filter
      if (this.selectedTypes.length && !this.selectedTypes.includes(t.type)) {
        return false;
      }

      // Direction filter
      if (this.direction === 'IN' && t.quantityDelta <= 0) return false;
      if (this.direction === 'OUT' && t.quantityDelta >= 0) return false;

      return true;
    });
  }

  iconFor(t: StockTransactionDTO): string {
    switch (t.type) {
      case 'RECEIPT': return 'add_circle';
      case 'SALE': return 'shopping_cart';
      case 'ADJUSTMENT': return 'tune';
      case 'TRANSFER_IN': return 'south_west';
      case 'TRANSFER_OUT': return 'north_east';
      case 'RETURN': return 'keyboard_return';
      default: return 'swap_horiz';
    }
  }

  typeClass(t: StockTransactionDTO): string {
    if (t.quantityDelta > 0) return 'tx-in';
    if (t.quantityDelta < 0) return 'tx-out';
    return '';
  }

  signedQty(tx: StockTransactionDTO): string {
    return tx.quantityDelta > 0
      ? `+${tx.quantityDelta}`
      : `${tx.quantityDelta}`;
  }
}