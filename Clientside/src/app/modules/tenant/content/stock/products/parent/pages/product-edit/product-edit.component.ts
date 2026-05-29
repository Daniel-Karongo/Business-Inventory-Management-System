import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';

import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import { ProductFormComponent } from '../../../components/product-form/product-form.component';

import { ProductService } from '../../services/product.service';

@Component({
  selector: 'app-product-edit',
  standalone: true,
  imports: [
    CommonModule,
    ProductFormComponent,
    MatSnackBarModule
  ],
  templateUrl: './product-edit.component.html',
  styleUrls: ['./product-edit.component.scss']
})
export class ProductEditComponent implements OnInit {

  productId!: string;

  product: any;

  loading = true;
  saving = false;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private productService: ProductService,
    private snackbar: MatSnackBar
  ) { }

  ngOnInit(): void {

    const id = this.route.snapshot.paramMap.get('id');

    if (!id) {
      this.router.navigate(['/app/stock']);
      return;
    }

    this.productId = id;

    this.loadProduct();
  }

  private loadProduct() {

    const deleted = history.state?.deleted === true;

    this.productService.getById(this.productId, deleted).subscribe({
      next: product => {
        this.product = product;
        this.loading = false;
      },
      error: () => {
        this.router.navigate(['/app/products']);
      }
    });
  }

  handleSubmit(formData: FormData) {

    if (this.saving || this.product?.deleted) return;

    this.saving = true;

    this.productService.update(
      this.productId,
      formData as any
    ).subscribe({
      next: () => {
        this.saving = false;

        this.snackbar.open(
          'Product updated',
          'Close',
          { duration: 3000 }
        );

        this.router.navigate([
          '/app/stock',
          this.productId
        ]);
      },
      error: () => {
        this.saving = false;

        this.snackbar.open(
          'Update failed',
          'Close',
          { duration: 3000 }
        );
      }
    });
  }

  cancel() {
    this.router.navigate([
      '/app/products',
      this.productId
    ]);
  }
}