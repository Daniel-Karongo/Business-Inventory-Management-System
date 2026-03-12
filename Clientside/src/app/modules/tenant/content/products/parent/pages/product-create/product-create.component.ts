import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { Router } from '@angular/router';

import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import { ProductFormComponent } from '../../../components/product-form/product-form.component';
import { ProductService } from '../../services/product.service';

@Component({
  selector: 'app-product-create',
  standalone: true,
  imports: [
    CommonModule,
    ProductFormComponent,
    MatSnackBarModule
  ],
  templateUrl: './product-create.component.html'
})
export class ProductCreateComponent {

  saving = false;

  constructor(
    private productService: ProductService,
    private snackbar: MatSnackBar,
    private router: Router
  ) {}

  handleSubmit(formData: FormData) {

    if (this.saving) return;

    this.saving = true;

    this.productService.fullCreate(formData).subscribe({
      next: product => {
        this.snackbar.open('Product created successfully', 'Close', { duration: 3000 });
        this.router.navigate(['/products', product.id]);
      },
      error: () => {
        this.saving = false;
        this.snackbar.open('Create failed', 'Close', { duration: 3000 });
      }
    });
  }

  cancel() {
    this.router.navigate(['/products']);
  }
}