import {
  BreakpointObserver
} from '@angular/cdk/layout';
import {
  CommonModule
} from '@angular/common';
import {
  ChangeDetectionStrategy,
  ChangeDetectorRef,
  Component,
  DestroyRef,
  Inject,
  OnInit,
  inject
} from '@angular/core';
import {
  takeUntilDestroyed
} from '@angular/core/rxjs-interop';
import {
  FormControl,
  ReactiveFormsModule
} from '@angular/forms';
import {
  DomSanitizer,
  SafeUrl
} from '@angular/platform-browser';
import {
  MatButtonModule
} from '@angular/material/button';
import {
  MatCheckboxModule
} from '@angular/material/checkbox';
import {
  MAT_DIALOG_DATA,
  MatDialog,
  MatDialogModule,
  MatDialogRef
} from '@angular/material/dialog';
import {
  MatFormFieldModule
} from '@angular/material/form-field';
import {
  MatIconModule
} from '@angular/material/icon';
import {
  MatInputModule
} from '@angular/material/input';
import {
  MatPaginatorModule,
  PageEvent
} from '@angular/material/paginator';
import {
  MatProgressSpinnerModule
} from '@angular/material/progress-spinner';
import {
  MatTableModule
} from '@angular/material/table';
import {
  MatTooltipModule
} from '@angular/material/tooltip';
import {
  debounceTime,
  distinctUntilChanged
} from 'rxjs';
import { LazyThumbnailDirective } from '../../../../../../../shared/directives/lazy-thumbnail.directive';
import { StockWorkspaceProduct } from '../../../models/product.model';
import { ProductService } from '../../parent/services/product.service';
import { ThumbnailCacheService } from '../../../workspace/services/thumbnails-cache.service';
import { FileViewerDialog } from '../../../../../../../shared/components/file-viewer/file-viewer.component';

@Component({
  standalone: true,
  selector: 'app-product-selector-dialog',
  changeDetection:
    ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatDialogModule,
    MatTableModule,
    MatCheckboxModule,
    MatInputModule,
    MatButtonModule,
    MatIconModule,
    MatTooltipModule,
    MatPaginatorModule,
    MatFormFieldModule,
    MatProgressSpinnerModule,
    LazyThumbnailDirective
  ],
  templateUrl:
    './product-selector-dialog.component.html',
  styleUrls: [
    './product-selector-dialog.component.scss'
  ]
})
export class ProductSelectorDialogComponent
  implements OnInit {

  private destroyRef =
    inject(DestroyRef);

  private breakpoint =
    inject(BreakpointObserver);

  private sanitizer =
    inject(DomSanitizer);

  private cdr =
    inject(ChangeDetectorRef);

  searchCtrl =
    new FormControl(
      '',
      { nonNullable: true }
    );

  products:
    StockWorkspaceProduct[] = [];

  selected =
    new Set<string>();

  loading = false;

  total = 0;
  page = 0;
  size = 20;

  isMobile = false;

  readonly pageSizeOptions = [
    10,
    20,
    50,
    100
  ];

  sortField:
    string | null =
    'updatedAt';

  sortDir:
    'asc' | 'desc' =
    'desc';

  displayedColumns = [
    'select',
    'thumbnail',
    'name',
    'category',
    'sku',
    'updated',
    'actions'
  ];

  constructor(
    private productService: ProductService,
    private thumbnailCache: ThumbnailCacheService,
    private dialog: MatDialog,
    private dialogRef: MatDialogRef<ProductSelectorDialogComponent>,
    @Inject(MAT_DIALOG_DATA)
    public data: any
  ) {
  }

  ngOnInit(): void {

    this.breakpoint
      .observe([
        '(max-width: 768px)'
      ])
      .pipe(
        takeUntilDestroyed(
          this.destroyRef
        )
      )
      .subscribe(result => {
        this.isMobile =
          result.matches;

        this.cdr.markForCheck();
      });

    this.load();

    this.searchCtrl.valueChanges
      .pipe(
        debounceTime(350),
        distinctUntilChanged(),
        takeUntilDestroyed(
          this.destroyRef
        )
      )
      .subscribe(() => {
        this.page = 0;
        this.load();
      });
  }

  private load(): void {

    this.loading = true;

    this.productService
      .search({
        page: this.page,
        size: this.size,
        keyword:
          this.searchCtrl.value?.trim()
          || undefined,
        deleted: false,
        sortBy:
          this.sortField ?? undefined,
        direction:
          this.sortDir
      })
      .subscribe({
        next: res => {

          this.products =
            res.content ?? [];

          this.total =
            res.totalElements ?? 0;

          this.loading =
            false;

          this.cdr.markForCheck();
        },
        error: () => {

          this.loading =
            false;

          this.products = [];
          this.total = 0;

          this.cdr.markForCheck();
        }
      });
  }

  sortBy(
    field: string
  ): void {

    if (
      this.sortField === field
    ) {
      this.sortDir =
        this.sortDir === 'asc'
          ? 'desc'
          : 'asc';
    } else {
      this.sortField = field;
      this.sortDir = 'asc';
    }

    this.load();
  }

  pageChanged(
    event: PageEvent
  ): void {

    this.page =
      event.pageIndex;

    this.size =
      event.pageSize;

    this.load();
  }

  toggle(
    product: StockWorkspaceProduct
  ): void {

    if (this.selected.has(product.id)) {
      this.selected.delete(product.id);
    } else {
      this.selected.add(product.id);
    }

    this.selected =
      new Set(this.selected);

    this.cdr.markForCheck();
  }

  isSelected(
    product:
      StockWorkspaceProduct
  ): boolean {

    return this.selected.has(
      product.id
    );
  }

  ensureThumbnailLoaded(
    product: StockWorkspaceProduct
  ): void {
    const thumbnailFileName =
      product.thumbnailFileName
      ?? product.primaryImageFileName;

    if (!thumbnailFileName) {
      return;
    }

    if (
      this.thumbnailCache.peekProduct(
        product.id
      )
    ) {
      return;
    }

    if (
      !this.thumbnailCache.beginProductLoad(
        product.id
      )
    ) {
      return;
    }

    this.productService
      .getSharedThumbnailBlob(
        thumbnailFileName,
        this.data?.branchId
      )
      .pipe(
        takeUntilDestroyed(
          this.destroyRef
        )
      )
      .subscribe({
        next: blob => {
          const url =
            URL.createObjectURL(blob);

          this.thumbnailCache.setProduct(
            product.id,
            url
          );

          this.thumbnailCache.finishProductLoad(
            product.id
          );

          this.products = [
            ...this.products
          ];

          this.cdr.markForCheck();
        },
        error: () => {
          this.thumbnailCache.finishProductLoad(
            product.id
          );
        }
      });
  }

  thumbnail(
    productId: string
  ): SafeUrl | null {

    const url =
      this.thumbnailCache
        .getProduct(
          productId
        );

    return url
      ? this.sanitizer
        .bypassSecurityTrustUrl(
          url
        )
      : null;
  }

  openThumbnail(
    product:
      StockWorkspaceProduct
  ): void {

    if (
      !product.primaryImageFileName
    ) {
      return;
    }

    this.productService
      .getImageBlob(
        product.id,
        product.primaryImageFileName,
        this.data?.branchId
      )
      .pipe(
        takeUntilDestroyed(
          this.destroyRef
        )
      )
      .subscribe(blob => {

        const url =
          URL.createObjectURL(
            blob
          );

        const ref =
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
                  src: url,
                  name:
                    product.name,
                  type:
                    'image'
                }
              }
            }
          );

        ref.afterClosed()
          .subscribe(() => {
            URL.revokeObjectURL(
              url
            );
          });
      });
  }

  confirm() {
    const picked = this.products.filter(p => this.selected.has(p.id));
    this.dialogRef.close(picked);
  }

  cancel(): void {
    this.dialogRef.close();
  }

  trackById(
    _: number,
    item:
      StockWorkspaceProduct
  ): string {
    return item.id;
  }
}