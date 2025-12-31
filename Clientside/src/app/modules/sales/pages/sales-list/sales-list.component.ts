import { Component, OnInit } from '@angular/core';
import { CommonModule, CurrencyPipe, DatePipe } from '@angular/common';
import { Router } from '@angular/router';

import { MatTableModule } from '@angular/material/table';
import { MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatChipsModule } from '@angular/material/chips';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { MatTooltipModule } from '@angular/material/tooltip';

import { SalesService } from '../../services/sales.service';

@Component({
  standalone: true,
  selector: 'app-sales-list',
  imports: [
    CommonModule,
    MatTableModule,
    MatPaginatorModule,
    MatChipsModule,
    MatIconModule,
    MatButtonModule,
    MatTooltipModule,
    CurrencyPipe,
    DatePipe
  ],
  templateUrl: './sales-list.component.html',
  styleUrls: ['./sales-list.component.scss']
})
export class SalesListComponent implements OnInit {

  displayedColumns = [
    'createdAt',
    'saleId',
    'status',
    'total',
    'paid',
    'balance',
    'actions'
  ];

  sales: any[] = [];
  total = 0;
  page = 0;
  size = 20;
  loading = false;

  constructor(
    private salesService: SalesService,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.load();
  }

  load(event?: PageEvent) {
    if (event) {
      this.page = event.pageIndex;
      this.size = event.pageSize;
    }

    this.loading = true;

    this.salesService.list({
      page: this.page,
      size: this.size
    }).subscribe({
      next: res => {
        this.sales = res.content ?? [];
        this.total = res.totalElements ?? 0;
        this.loading = false;
      },
      error: () => this.loading = false
    });
  }

  paidAmount(sale: any): number {
    return (sale.payments || [])
      .filter((p: any) => p.status === 'SUCCESS')
      .reduce((sum: number, p: any) => sum + Number(p.amount), 0);
  }

  balance(sale: any): number {
    return Number(sale.totalAmount) - this.paidAmount(sale);
  }

  view(sale: any) {
    this.router.navigate(['/sales', sale.id]);
  }

  create() {
    this.router.navigate(['/sales/new']);
  }
}