import {
  Component,
  Input,
  OnInit
} from '@angular/core';

import {
  CommonModule
} from '@angular/common';

import {
  Router
} from '@angular/router';

import {
  MatButtonModule
} from '@angular/material/button';

import {
  MatIconModule
} from '@angular/material/icon';

import {
  MatTooltipModule
} from '@angular/material/tooltip';

import {
  MatSnackBar
} from '@angular/material/snack-bar';

import {
  MatDialog
} from '@angular/material/dialog';

import {
  Product
} from '../../../../models/product.model';

import {
  ProductVariant
} from '../../../../models/product-variant.model';

import {
  ProductVariantService,
  VariantFilterType
} from '../../services/product-variant.service';

import {
  VariantDetailsDialogComponent
} from '../variant-details/variant-details-dialog.component';

import {
  VariantFormComponent
} from '../variant-form/variant-form.component';

import {
  VariantBarcodeDialogComponent
} from '../variant-barcode/variant-barcode-dialog.component';

import {
  ProductVariantImageAdapter
} from '../../services/product-variant-image.adapter';
import { EntityImageManagerComponent } from '../../../../../../../../shared/components/entity-image-manager/entity-image-manager.component';
import { ReasonDialogComponent } from '../../../../../../../../shared/components/reason-dialog/reason-dialog.component';

@Component({
  selector: 'app-variant-summary',
  standalone: true,
  imports: [
    CommonModule,
    MatButtonModule,
    MatIconModule,
    MatTooltipModule
  ],
  templateUrl:
    './variant-summary.component.html',
  styleUrls: [
    './variant-summary.component.scss'
  ]
})
export class VariantSummaryComponent
  implements OnInit {

  @Input({ required: true })
  product!: Product;

  @Input()
  branchId?: string;

  loading = true;

  variants: ProductVariant[] = [];
  currentFilter: VariantFilterType = 'ACTIVE';

  constructor(
    private router: Router,
    private variantService: ProductVariantService,
    private snackBar: MatSnackBar,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {
    this.load();
  }

  load(): void {

    this.loading = true;

    this.variantService
      .forProduct(
        this.product.id,
        this.currentFilter
      )
      .subscribe({
        next: variants => {

          this.variants =
            variants ?? [];

          this.loading = false;
        },
        error: (err) => {

          this.loading = false;

          this.snackBar.open(
            this.getErrorMessage(err),
            'Close',
            {
              duration: 5000
            }
          );

        }
      });
  }

  setFilter(
    filter: VariantFilterType
  ): void {

    if (
      this.currentFilter === filter
    ) {
      return;
    }

    this.currentFilter =
      filter;

    this.load();
  }

  createSale(
    variant: ProductVariant
  ): void {

    this.router.navigate(
      ['/app/sales/new'],
      {
        state: {
          inventorySeed: {
            productId:
              this.product.id,
            productName:
              this.product.name,
            variantId:
              variant.id,
            branchId:
              this.branchId,
            rowType:
              'VARIANT'
          }
        }
      }
    );
  }

  view(
    variant: ProductVariant
  ): void {

    this.dialog.open(
      VariantDetailsDialogComponent,
      {
        panelClass:
          'enterprise-dialog',
        width:
          'min(1100px, 94vw)',
        maxWidth:
          '94vw',
        maxHeight:
          '92vh',
        autoFocus: false,
        restoreFocus: false,
        data: {
          variant,
          branchId:
            this.branchId
        }
      }
    );
  }

  edit(
    variant: ProductVariant
  ): void {

    const ref =
      this.dialog.open(
        VariantFormComponent,
        {
          panelClass:
            'enterprise-dialog',
          width:
            '700px',
          maxWidth:
            '95vw',
          maxHeight:
            '90vh',
          data:
            variant
        }
      );

    ref.afterClosed()
      .subscribe(updated => {

        if (!updated) {
          return;
        }

        this.snackBar.open(
          'Variant updated',
          'Close',
          {
            duration: 2000
          }
        );

        this.load();
      });
  }

  manageImages(
    variant: ProductVariant
  ): void {

    const ref =
      this.dialog.open(
        EntityImageManagerComponent,
        {
          panelClass:
            'enterprise-dialog',
          width:
            '1200px',
          maxWidth:
            '95vw',
          maxHeight:
            '92vh'
        }
      );

    const adapter =
      ProductVariantImageAdapter(
        this.variantService
      );

    ref.componentInstance.entityId =
      variant.id;

    ref.componentInstance.adapter =
      adapter;

    ref.componentInstance.allowHardDelete =
      true;

    adapter.onChange = () => {
      this.load();
    };

    adapter.onThumbnailUpdated =
      adapter.onChange;
  }

  showBarcode(
    variant: ProductVariant
  ): void {

    this.dialog.open(
      VariantBarcodeDialogComponent,
      {
        panelClass:
          'enterprise-dialog',
        width:
          '700px',
        maxWidth:
          '95vw',
        data: {
          variantId:
            variant.id,
          variantName:
            variant.classification
        }
      }
    );
  }

  delete(
    variant: ProductVariant
  ): void {

    const ref =
      this.dialog.open(
        ReasonDialogComponent,
        {
          width: '500px',
          panelClass: 'enterprise-dialog',
          data: {
            title:
              'Delete Variant',
            message:
              `Delete variant "${variant.classification}"?`,
            action:
              'DELETE',
            requireReason:
              false,
            allowCustomReason:
              true,
            reasons: [
              'Duplicate variant',
              'Obsolete variant',
              'Incorrect creation',
              'Discontinued'
            ]
          }
        }
      );

    ref.afterClosed()
      .subscribe(result => {

        if (!result?.confirmed) {
          return;
        }

        this.variantService
          .remove(
            variant.id,
            result.reason
          )
          .subscribe({
            next: () => {

              this.snackBar.open(
                'Variant deleted',
                'Close',
                {
                  duration: 2000
                }
              );

              this.load();
            },

            error: (err) => {

              this.snackBar.open(
                this.getErrorMessage(err),
                'Close',
                {
                  duration: 5000
                }
              );

            }
          });
      });
  }

  restore(
    variant: ProductVariant
  ): void {

    const ref =
      this.dialog.open(
        ReasonDialogComponent,
        {
          width: '500px',
          panelClass: 'enterprise-dialog',
          data: {
            title:
              'Restore Variant',
            message:
              `Restore variant "${variant.classification}"?`,
            action:
              'RESTORE',
            requireReason:
              false,
            allowCustomReason:
              true
          }
        }
      );

    ref.afterClosed()
      .subscribe(result => {

        if (!result?.confirmed) {
          return;
        }

        this.variantService
          .restore(
            variant.id,
            result.reason
          )
          .subscribe({
            next: () => {

              this.snackBar.open(
                'Variant restored',
                'Close',
                {
                  duration: 2000
                }
              );

              this.load();
            },

            error: (err) => {

              this.snackBar.open(
                this.getErrorMessage(err),
                'Close',
                {
                  duration: 5000
                }
              );

            }
          });
      });
  }

  hardDelete(
    variant: ProductVariant
  ): void {

    const ref =
      this.dialog.open(
        ReasonDialogComponent,
        {
          width: '500px',
          panelClass: 'enterprise-dialog',
          data: {
            title:
              'Permanent Delete',
            message:
              `Permanently delete variant "${variant.classification}"?`,
            action:
              'DELETE',
            requireReason:
              false,
            allowCustomReason:
              true
          }
        }
      );

    ref.afterClosed()
      .subscribe(result => {

        if (!result?.confirmed) {
          return;
        }

        this.variantService
          .hardDelete(
            variant.id,
            result.reason
          )
          .subscribe({
            next: () => {

              this.snackBar.open(
                'Variant permanently deleted',
                'Close',
                {
                  duration: 2500
                }
              );

              this.load();
            },

            error: (err) => {

              this.snackBar.open(
                this.getErrorMessage(err),
                'Close',
                {
                  duration: 5000
                }
              );

            }
          });
      });
  }

  private getErrorMessage(err: any): string {

    return (
      err?.error?.message ||
      err?.error?.error ||
      err?.error?.text ||
      err?.error ||
      err?.message ||
      'Operation failed'
    );

  }
}