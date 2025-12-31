import { Component, Input, OnInit } from '@angular/core';
import { CommonModule, CurrencyPipe, DatePipe } from '@angular/common';
import { ActivatedRoute, RouterModule } from '@angular/router';

import { MatButtonModule } from '@angular/material/button';

import { SalesService } from '../../services/sales.service';

@Component({
  standalone: true,
  selector: 'app-sale-receipt',
  imports: [
    CommonModule,
    CurrencyPipe,
    DatePipe,
    MatButtonModule,
    RouterModule
  ],
  templateUrl: './sale-receipt.component.html',
  styleUrls: ['./sale-receipt.component.scss']
})
export class SaleReceiptComponent implements OnInit {

  @Input() sale: any;
  loading = true;

  constructor(
    private route: ActivatedRoute,
    private salesService: SalesService
  ) { }

  ngOnInit(): void {
    const id = this.route.snapshot.paramMap.get('id');
    if (!id) return;

    this.salesService.get(id).subscribe(sale => {
      this.sale = sale;
      this.loading = false;

      // 🔥 Auto-print once data is ready
      setTimeout(() => window.print(), 300);
    });
  }

  print() {
    window.print();
  }
}