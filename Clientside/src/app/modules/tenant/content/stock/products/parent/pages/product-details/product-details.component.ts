import {
  Component,
  OnDestroy,
  OnInit,
  ViewChild
} from '@angular/core';

import {
  CommonModule
} from '@angular/common';

import {
  ActivatedRoute,
  Router,
  RouterModule
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
  MatChipsModule
} from '@angular/material/chips';

import {
  MatMenuModule
} from '@angular/material/menu';

import {
  BreakpointObserver
} from '@angular/cdk/layout';

import {
  MatSnackBar
} from '@angular/material/snack-bar';

import {
  ProductService
} from '../../services/product.service';

import {
  Product
} from '../../../../models/product.model';

import {
  WorkflowShellComponent
} from '../../../../../../../../shared/layout/workflow-shell/workflow-shell.component';

import {
  ProductInventorySummaryComponent
} from '../../components/product-inventory-summary/product-inventory-summary.component';

import {
  VariantSummaryComponent
} from '../../../variant/components/variant-summary/variant-summary.component';

import {
  ProductAuditsComponent
} from '../../components/product-audits/product-audits.component';

import {
  EntityImageManagerComponent
} from '../../../../../../../../shared/components/entity-image-manager/entity-image-manager.component';

import { MatDialog } from '@angular/material/dialog';
import { FileViewerDialog } from '../../../../../../../../shared/components/file-viewer/file-viewer.component';
import { ReasonDialogComponent } from '../../../../../../../../shared/components/reason-dialog/reason-dialog.component';
import {
  ProductImageAdapter
} from '../../services/product-image.adapter';

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
    MatMenuModule,

    WorkflowShellComponent,

    ProductInventorySummaryComponent,
    VariantSummaryComponent,
    ProductAuditsComponent,
    EntityImageManagerComponent
  ],
  templateUrl: './product-details.component.html',
  styleUrls: ['./product-details.component.scss']
})
export class ProductDetailsComponent
  implements OnInit, OnDestroy {

  @ViewChild(EntityImageManagerComponent)
  private imageManager?: EntityImageManagerComponent;

  @ViewChild(VariantSummaryComponent)
  private variantSummary?: VariantSummaryComponent;

  @ViewChild(ProductAuditsComponent)
  private audits?: ProductAuditsComponent;

  product?: Product;

  loading = true;

  processing = false;

  isMobile = false;

  thumbnailUrl?: string;

  fullImageUrl?: string;

  imageAdapter;

  activeTab:
    | 'overview'
    | 'inventory'
    | 'variants'
    | 'images'
    | 'audits'
    = 'overview';

  readonly tabs: {
    label: string;
    key:
    | 'overview'
    | 'inventory'
    | 'variants'
    | 'images'
    | 'audits';
    icon: string;
  }[] = [
      {
        label: 'Overview',
        key: 'overview',
        icon: 'visibility'
      },
      {
        label: 'Inventory',
        key: 'inventory',
        icon: 'inventory_2'
      },
      {
        label: 'Variants',
        key: 'variants',
        icon: 'category'
      },
      {
        label: 'Images',
        key: 'images',
        icon: 'image'
      },
      {
        label: 'Audits',
        key: 'audits',
        icon: 'history'
      }
    ];

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private productService: ProductService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog,
    private breakpointObserver: BreakpointObserver
  ) {
    this.imageAdapter =
      ProductImageAdapter(
        this.productService
      );
    this.imageAdapter.onChange = () => {
      this.reloadProduct();
    };

    this.imageAdapter.onThumbnailUpdated = () => {
      this.reloadProduct();
    };
  }

  ngOnInit(): void {

    this.breakpointObserver
      .observe('(max-width: 768px)')
      .subscribe(result => {
        this.isMobile =
          result.matches;
      });

    const id =
      this.route.snapshot.paramMap.get('id');

    if (!id) {
      this.router.navigate(['/app/stock']);
      return;
    }

    this.productService
      .getById(id, false)
      .subscribe({
        next: product => {
          this.product = product;
          this.loading = false;
          this.loadThumbnail();
        },
        error: () => {
          this.productService
            .getById(id, true)
            .subscribe({
              next: product => {
                this.product = product;
                this.loading = false;
                this.loadThumbnail();
              },
              error: () => {
                this.router.navigate(['/app/stock']);
              }
            });
        }
      });
  }

  ngOnDestroy(): void {

    if (
      this.thumbnailUrl
    ) {
      URL.revokeObjectURL(
        this.thumbnailUrl
      );
    }
  }

  private loadThumbnail() {

    if (!this.product?.id) {
      return;
    }

    this.productService
      .getThumbnail(
        this.product.id
      )
      .subscribe({
        next: blob => {

          this.thumbnailUrl =
            URL.createObjectURL(
              blob
            );
        },
        error: () => {
          this.thumbnailUrl =
            undefined;
        }
      });
  }

  openThumbnail(): void {

    if (
      !this.product?.images?.length
    ) {
      return;
    }

    const primaryImage =
      this.product.images.find(
        image =>
          image.primaryImage
          &&
          !image.deleted
      )
      ??
      this.product.images.find(
        image => !image.deleted
      );

    if (!primaryImage) {
      return;
    }

    this.productService
      .getImageBlob(
        this.product.id,
        primaryImage.fileName
      )
      .subscribe(blob => {

        const imageUrl =
          URL.createObjectURL(blob);

        this.dialog.open(
          FileViewerDialog,
          {
            panelClass:
              'enterprise-dialog',
            width: '90vw',
            maxWidth: '90vw',
            height: '90vh',
            maxHeight: '90vh',
            data: {
              preview: {
                src: imageUrl,
                name:
                  primaryImage.fileName,
                type: 'image'
              }
            }
          }
        )
          .afterClosed()
          .subscribe(() => {
            URL.revokeObjectURL(
              imageUrl
            );
          });

      });
  }

  openThumbnailOrUpload(): void {

    if (this.thumbnailUrl) {
      this.openThumbnail();
      return;
    }

    this.activeTab = 'images';

    setTimeout(() => {
      this.imageManager?.openUploadDialog();
    });
  }

  private reloadProduct(): void {
    if (!this.product?.id) {
      return;
    }

    this.productService
      .getById(this.product.id, null)
      .subscribe({
        next: product => {
          this.product = product;

          this.loadThumbnail();

          this.variantSummary?.load();
          this.imageManager?.reload();
          this.audits?.reload();
        }
      });
  }
  
  selectTab(
    tab:
      | 'overview'
      | 'inventory'
      | 'variants'
      | 'images'
      | 'audits'
  ) {
    this.activeTab = tab;
  }

  softDeleteProduct() {

    if (
      !this.product ||
      this.product.deleted
    ) {
      return;
    }

    this.dialog.open(
      ReasonDialogComponent,
      {
        width: '500px',
        data: {
          title: 'Delete Product',
          message: 'Select a reason for deleting this product.',
          action: 'DELETE',
          confirmText: 'Delete',
          requireReason: false,
          reasons: [
            'Discontinued',
            'Supplier no longer available',
            'Duplicate product',
            'Incorrect product setup',
            'Inventory migration',
            'Seasonal product removed'
          ],
          allowCustomReason: true
        }
      }
    )
      .afterClosed()
      .subscribe(result => {

        if (!result?.confirmed) {
          return;
        }

        this.processing = true;

        this.productService
          .softDelete(
            this.product!.id,
            result.reason
          )
          .subscribe({
            next: () => {
              this.processing = false;
              this.reloadProduct();
              this.processing = false;

              this.snackbar.open(
                'Product deleted',
                'Close',
                { duration: 3000 }
              );
            },
            error: () => {
              this.processing = false;
            }
          });

      });
  }

  restoreProduct() {

    if (
      !this.product ||
      !this.product.deleted
    ) {
      return;
    }

    this.dialog.open(
      ReasonDialogComponent,
      {
        width: '500px',
        data: {
          title: 'Restore Product',
          message:
            'Select a reason for restoring this product.',
          action: 'RESTORE',
          confirmText: 'Restore',
          requireReason: false,
          reasons: [
            'Product reintroduced',
            'Deletion was accidental',
            'Supplier resumed supply',
            'Inventory restored',
            'Operational requirement'
          ],
          allowCustomReason: true
        }
      }
    )
      .afterClosed()
      .subscribe(result => {

        if (!result?.confirmed) {
          return;
        }

        this.processing = true;

        this.productService
          .restore(
            this.product!.id,
            result.reason
          )
          .subscribe({
            next: () => {
              this.processing = false;
              this.reloadProduct();
              this.processing = false;

              this.snackbar.open(
                'Product restored',
                'Close',
                { duration: 3000 }
              );
            },
            error: () => {
              this.processing = false;
            }
          });

      });
  }

  hardDeleteProduct() {

    if (
      !this.product ||
      !this.product.deleted
    ) {
      return;
    }

    this.dialog.open(
      ReasonDialogComponent,
      {
        width: '500px',
        data: {
          title: 'Permanent Delete Product',
          message:
            'This action cannot be undone.',
          action: 'DELETE',
          confirmText: 'Delete Forever',
          requireReason: false,
          reasons: [
            'Duplicate record',
            'Corrupt product data',
            'Testing data cleanup',
            'Legal/compliance request',
            'Data retention policy'
          ],
          allowCustomReason: true
        }
      }
    )
      .afterClosed()
      .subscribe(result => {

        if (!result?.confirmed) {
          return;
        }

        this.processing = true;

        this.productService
          .hardDelete(
            this.product!.id,
            result.reason
          )
          .subscribe({
            next: () => {

              this.router.navigate(
                ['/app/stock']
              );
            },
            error: () => {
              this.processing = false;
            }
          });

      });
  }

  get statusClass(): string {

    if (!this.product) {
      return 'loading';
    }

    return this.product.deleted
      ? 'deleted'
      : 'active';
  }

  get statusLabel(): string {

    if (!this.product) {
      return 'Loading';
    }

    return this.product.deleted
      ? 'Deleted'
      : 'Active';
  }
}