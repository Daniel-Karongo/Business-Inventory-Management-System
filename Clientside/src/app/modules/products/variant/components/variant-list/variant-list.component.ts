import { Component, Input, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatTableModule } from '@angular/material/table';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';

import { Product } from '../../../parent/models/product.model';
import { ProductVariant } from '../../models/product-variant.model';
import { ProductVariantService } from '../../services/product-variant.service';
import { VariantFormComponent } from '../variant-form/variant-form.component';
import { InventoryService } from '../../../../inventory/services/inventory.service';

@Component({
  selector: 'app-variant-list',
  standalone: true,
  imports: [
    CommonModule,
    MatTableModule,
    MatButtonModule,
    MatIconModule,
    MatTooltipModule,
    MatSnackBarModule,
    MatDialogModule
  ],
  templateUrl: './variant-list.component.html',
  styleUrls: ['./variant-list.component.scss']
})
export class VariantListComponent implements OnInit {

  @Input({ required: true }) product!: Product;

  variants: ProductVariant[] = [];
  loading = true;
  inventoryMap = new Map<string, number>();

  displayedColumns = [
    'classification',
    'sku',
    'minPrice',
    'avgPrice',
    'stock',
    'actions'
  ];

  constructor(
    private variantService: ProductVariantService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog,
    private inventoryService: InventoryService
  ) { }

  ngOnInit(): void {
    this.loadVariants();
  }

  loadVariants() {
    this.loading = true;

    this.variantService.forProduct(this.product.id).subscribe({
      next: variants => {
        this.variants = variants || [];

        this.variants.forEach(v => {
          this.inventoryService.getVariantAcrossBranches(v.id).subscribe(res => {
            const rows = res?.data || [];

            const totalOnHand = rows.reduce(
              (sum: number, r: any) => sum + (r.quantityOnHand ?? 0),
              0
            );

            this.inventoryMap.set(v.id, totalOnHand);
          });
        });

        this.loading = false;
      },
      error: () => (this.loading = false)
    });
  }

  edit(v: ProductVariant) {
    const ref = this.dialog.open(VariantFormComponent, {
      width: '420px',
      data: v
    });

    ref.afterClosed().subscribe(updated => {
      if (!updated) return;
      this.snackbar.open('Variant updated', 'Close', { duration: 2000 });
      this.loadVariants();
    });
  }

  delete(v: ProductVariant) {
    const ok = confirm(`Delete variant "${v.classification}" permanently?`);
    if (!ok) return;

    this.variantService.delete(v.id).subscribe({
      next: () => {
        this.snackbar.open('Variant deleted', 'Close', { duration: 2000 });
        this.loadVariants();
      },
      error: () => {
        this.snackbar.open('Delete failed', 'Close', { duration: 3000 });
      }
    });
  }
}