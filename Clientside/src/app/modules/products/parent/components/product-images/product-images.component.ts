import { Component, Input, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar } from '@angular/material/snack-bar';
import JSZip from 'jszip';

import { Product } from '../../models/product.model';
import { ProductService } from '../../services/product.service';
import { FileViewerDialog } from '../../../../../shared/components/file-viewer/file-viewer.component';
import { MatDialog } from '@angular/material/dialog';

@Component({
  selector: 'app-product-images',
  standalone: true,
  imports: [CommonModule, MatButtonModule, MatIconModule],
  templateUrl: './product-images.component.html',
  styleUrls: ['./product-images.component.scss']
})
export class ProductImagesComponent implements OnInit {

  @Input({ required: true }) product!: Product;

  loading = true;
  images: { name: string; url: string }[] = [];
  barcodeImageUrl?: string;

  constructor(
    private productService: ProductService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {
    this.loadImages();
    this.loadBarcode();
  }

  /* ================= LOAD ================= */

  loadImages() {
    this.productService
      .downloadImagesZip(this.product.id, false)
      .subscribe({
        next: async blob => {
          const zip = await JSZip.loadAsync(blob);
          this.images = [];

          for (const name of Object.keys(zip.files)) {
            const file = zip.files[name];
            if (!file.dir) {
              const fileBlob = await file.async('blob');
              const url = URL.createObjectURL(fileBlob);
              this.images.push({ name, url });
            }
          }

          this.loading = false;
        },
        error: () => {
          this.snackbar.open('Failed to load product images', 'Close', { duration: 3000 });
          this.loading = false;
        }
      });
  }

  loadBarcode() {
    if (!this.product.barcode) return;

    this.productService.getBarcodeImage(this.product.barcode).subscribe(blob => {
      this.barcodeImageUrl = URL.createObjectURL(blob);
    });
  }

  /* ================= ACTIONS ================= */

  viewImage(img: { name: string; url: string }) {
    this.dialog.open(FileViewerDialog, {
      width: '80%',
      maxWidth: '1100px',
      data: {
        preview: {
          src: img.url,
          name: img.name,
          type: 'image'
        }
      }
    });
  }

  viewBarcode() {
    if (!this.barcodeImageUrl) return;

    this.dialog.open(FileViewerDialog, {
      width: '80%',
      maxWidth: '1100px',
      data: {
        preview: {
          src: this.barcodeImageUrl,
          name: 'Product Barcode',
          type: 'image'
        }
      }
    });
  }

  downloadBarcodePdf() {
    if (!this.product.barcode) return;

    this.productService.getBarcodePdf(this.product.barcode).subscribe(blob => {
      const url = URL.createObjectURL(blob);
      window.open(url, '_blank');
    });
  }
}