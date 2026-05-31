import { Component, Input, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatIconModule } from '@angular/material/icon';

import { ProductService } from '../../services/product.service';
import { ProductAudit } from '../../../../models/product.model';

@Component({
  selector: 'app-product-audits',
  standalone: true,
  imports: [CommonModule, MatIconModule],
  templateUrl: './product-audits.component.html',
  styleUrls: ['./product-audits.component.scss']
})
export class ProductAuditsComponent implements OnInit {

  @Input({ required: true }) productId!: string;

  audits: ProductAudit[] = [];
  loading = true;

  constructor(private productService: ProductService) {}

  ngOnInit(): void {
    this.productService.getAudits(this.productId).subscribe({
      next: a => {
        this.audits = a || [];
        this.loading = false;
      },
      error: () => this.loading = false
    });
  }
}
