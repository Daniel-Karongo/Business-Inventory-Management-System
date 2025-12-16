import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ActivatedRoute, Router, RouterModule } from '@angular/router';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatChipsModule } from '@angular/material/chips';

import { ProductService } from '../../services/product.service';
import { Product } from '../../models/product.model';
import { VariantListComponent } from '../../../variant/components/variant-list/variant-list.component';
import { ProductAuditsComponent } from '../../components/product-audits/product-audits.component';
import { VariantSummaryComponent } from '../../components/variant-summary/variant-summary.component';
import { ProductImagesComponent } from '../../components/product-images/product-images.component';

@Component({
  selector: 'app-product-details',
  standalone: true,
  imports: [
    CommonModule,
    RouterModule,
    MatButtonModule,
    MatIconModule,
    MatTooltipModule,
    MatChipsModule,
    VariantSummaryComponent,
    ProductAuditsComponent,
    ProductImagesComponent
  ],
  templateUrl: './product-details.component.html',
  styleUrls: ['./product-details.component.scss']
})
export class ProductDetailsComponent implements OnInit {

  product?: Product;
  loading = true;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private productService: ProductService
  ) { }

  ngOnInit(): void {
    const id = this.route.snapshot.paramMap.get('id');

    // ✅ This survives refresh & back/forward
    const deleted = history.state?.deleted === true;

    if (!id) {
      this.router.navigate(['/products']);
      return;
    }

    this.productService.getById(id, deleted).subscribe({
      next: p => {
        this.product = p;
        this.loading = false;
      },
      error: () => this.router.navigate(['/products'])
    });
  }


  edit() {
    if (!this.product) return;
    this.router.navigate(['/products', this.product.id, 'edit']);
  }
}