import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';

import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import { ProductService } from '../../services/product.service';

@Component({
  selector: 'app-product-bulk-create',
  standalone: true,
  imports: [
    CommonModule,
    MatButtonModule,
    MatIconModule,
    MatSnackBarModule
  ],
  templateUrl: './product-bulk-create.component.html',
  styleUrls: ['./product-bulk-create.component.scss']
})
export class ProductBulkCreateComponent {

  file?: File;
  uploading = false;

  constructor(
    private productService: ProductService,
    private snackbar: MatSnackBar,
    private router: Router
  ) {}

  onFileSelected(event: Event) {
    const input = event.target as HTMLInputElement;
    const file = input.files?.[0];

    if (!file) return;

    this.file = file;
  }

  upload() {
    if (!this.file || this.uploading) return;

    const fd = new FormData();
    fd.append('file', this.file);

    this.uploading = true;

    this.productService.bulkCreate(fd).subscribe({
      next: () => {
        this.snackbar.open('Products uploaded successfully', 'Close', {
          duration: 2500
        });
        this.router.navigate(['/products']);
      },
      error: () => {
        this.uploading = false;
        this.snackbar.open(
          'Bulk upload failed. Check file format.',
          'Close',
          { duration: 3500 }
        );
      }
    });
  }

  cancel() {
    this.router.navigate(['/products']);
  }
}