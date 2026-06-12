interface SaleSeed {
  productId: string;
  productName: string;
  variantId?: string | null;
  branchId?: string | null;
  rowType?:
  | 'PRODUCT'
  | 'VARIANT'
  | 'INVENTORY';
}

import {
  CommonModule,
  CurrencyPipe
} from '@angular/common';

import {
  Component,
  DestroyRef,
  ElementRef,
  HostListener,
  OnInit,
  ViewChild,
  inject
} from '@angular/core';

import {
  FormArray,
  FormBuilder,
  FormControl,
  FormGroup,
  ReactiveFormsModule,
  Validators
} from '@angular/forms';

import {
  debounceTime,
  distinctUntilChanged,
  filter,
  forkJoin,
  of,
  switchMap
} from 'rxjs';

import {
  takeUntilDestroyed
} from '@angular/core/rxjs-interop';

import {
  Router
} from '@angular/router';

import {
  HttpClient
} from '@angular/common/http';

import {
  MatButtonModule
} from '@angular/material/button';

import {
  MatDialog
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
  MatSnackBar
} from '@angular/material/snack-bar';

import {
  MatSelectModule
} from '@angular/material/select';

import {
  MatTooltipModule
} from '@angular/material/tooltip';

import {
  environment
} from '../../../../../../../environments/environment';

import {
  AuthService
} from '../../../../../auth/services/auth.service';

import {
  Product
} from '../../../stock/models/product.model';

import {
  AllocationDetail
} from '../../../stock/models/allocation.model';

import {
  PackagingDTO
} from '../../../stock/models/packaging.model';

import {
  PricingAdjustment
} from '../../../stock/models/pricing.model';

import {
  SaleLinePreviewResponse
} from '../../../stock/models/sale-preview.model';

import {
  SaleRequest
} from '../../../stock/models/sale.model';

import {
  SellableVariantDTO,
  SellableVariantRequest
} from '../../../stock/models/sellable.model';

import {
  Navigation
} from '@angular/router';

import {
  ProductVariantService
} from '../../../stock/products/variant/services/product-variant.service';

import {
  ProductVariantPackagingService
} from '../../../stock/products/variant/services/product-variant-packaging.service';

import {
  ProductVariant
} from '../../../stock/models/product-variant.model';

import {
  BranchService
} from '../../../branches/services/branch.service';

import {
  CustomerService
} from '../../../customers/services/customer.service';

import {
  ProductService
} from '../../../stock/products/parent/services/product.service';

import {
  BatchAllocationDialogComponent
} from '../../dialogs/batch-allocation-dialog/batch-allocation-dialog.component';

import {
  ProductSelectorDialogComponent
} from '../../../stock/products/components/product-selector-dialog/product-selector-dialog.component';

import {
  BarcodeService
} from '../../services/barcode.service';

import {
  PosBarcodeService
} from '../../services/pos-barcode.service';

import {
  SalePreviewService
} from '../../services/sale-preview.service';

import {
  SalesService
} from '../../services/sales.service';

import {
  SellableService
} from '../../services/sellable.service';
import { BranchContextService } from '../../../../../../core/services/branch-context.service';

interface SaleLinePreviewState {
  loading: boolean;
  resolved: boolean;
  preview?: SaleLinePreviewResponse;
  warnings: string[];
  adjustments: PricingAdjustment[];
  allocations: AllocationDetail[];
}

interface SaleLineFormValue {
  productId: string | null;
  productName: string | null;
  productVariantId: string | null;
  packagingId: string | null;
  branchId: string | null;
  quantity: number;
  batchSelections: {
    batchId: string;
    quantity: number;
  }[] | null;
  requestedUnitPrice: number | null;
  overrideReason: string | null;
}

type SaleItemForm = FormGroup<{
  productId: FormControl<string | null>;
  productName: FormControl<string | null>;
  productVariantId: FormControl<string | null>;
  packagingId: FormControl<string | null>;
  branchId: FormControl<string | null>;
  quantity: FormControl<number>;
  batchSelections: FormControl<{
    batchId: string;
    quantity: number;
  }[] | null>;
  requestedUnitPrice: FormControl<number | null>;
  overrideReason: FormControl<string | null>;
}>;

@Component({
  standalone: true,
  selector: 'app-sale-create',
  imports: [
    CommonModule,
    ReactiveFormsModule,
    CurrencyPipe,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatButtonModule,
    MatIconModule,
    MatTooltipModule
  ],
  templateUrl: './sale-create.component.html',
  styleUrls: ['./sale-create.component.scss']
})
export class SaleCreateComponent implements OnInit {

  @ViewChild('barcodeInput')
  barcodeInput!: ElementRef<HTMLInputElement>;

  private readonly destroyRef =
    inject(DestroyRef);

  private readonly fb =
    inject(FormBuilder);

  private readonly router =
    inject(Router);

  private readonly dialog =
    inject(MatDialog);

  private readonly snackBar =
    inject(MatSnackBar);

  private readonly http =
    inject(HttpClient);

  private readonly authService =
    inject(AuthService);

  private readonly branchService =
    inject(BranchService);

  private readonly productService =
    inject(ProductService);

  private readonly customerService =
    inject(CustomerService);

  private readonly salesService =
    inject(SalesService);

  private readonly sellableService =
    inject(SellableService);

  private readonly previewService =
    inject(SalePreviewService);

  private readonly barcodeService =
    inject(BarcodeService);

  private readonly posBarcodeService =
    inject(PosBarcodeService);

  private readonly branchContext =
    inject(BranchContextService);

  private readonly variantService =
    inject(ProductVariantService);

  private readonly packagingService =
    inject(ProductVariantPackagingService);

  barcodeCtrl =
    new FormControl('');

  scanning = false;

  submitting = false;

  products: Product[] = [];

  branches: any[] = [];

  packagingMap: Record<number, PackagingDTO[]> = {};

  variantMap: Record<number, SellableVariantDTO[]> = {};

  previewStateMap: Record<number, SaleLinePreviewState> = {};

  displayedColumns = [
    'product',
    'variant',
    'packaging',
    'branch',
    'quantity'
  ];

  readonly form = this.fb.group({
    customer: this.fb.group({
      name: this.fb.control<string | null>(null),
      phone: this.fb.control<string | null>(null),
      email: this.fb.control<string | null>(null)
    }),
    items: this.fb.array<SaleItemForm>([])
  });

  defaultBranchId: string | null = null;

  ngOnInit(): void {

    const me =
      this.authService.getSnapshot();

    this.defaultBranchId =
      me?.branchId ?? null;

    this.addLine();

    this.processIncomingSeed();

    this.loadInitialDependencies();

    this.setupCustomerLookup();
  }

  get customerGroup() {
    return this.form.controls.customer;
  }

  get items(): FormArray<SaleItemForm> {
    return this.form.controls.items;
  }

  get showBranchField(): boolean {

    return this.branches.length > 1;
  }

  private loadInitialDependencies(): void {

    forkJoin({

      products:
        this.productService.getAll(),

      branches:
        this.branchService.getAllLegacy()

    })
      .pipe(
        takeUntilDestroyed(
          this.destroyRef
        )
      )
      .subscribe({

        next: result => {

          this.products =
            result.products;

          this.branches =
            result.branches;

          if (
            result.branches.length === 1
          ) {

            this.defaultBranchId =
              result.branches[0].id;

            this.items.controls
              .forEach(control =>
                control.patchValue({
                  branchId:
                    this.defaultBranchId
                })
              );
          }

          this.processIncomingSeed();
        }
      });
  }

  private processIncomingSeed(): void {

    const navigation =
      this.router.getCurrentNavigation();

    const seed =
      (
        navigation?.extras?.state?.[
        'inventorySeed'
        ]
        ??
        history.state?.inventorySeed
      ) as SaleSeed | undefined;

    if (!seed) {
      return;
    }

    console.log('SALE SEED', seed);

    const row =
      this.items.at(0);

    row.patchValue({
      productId:
        seed.productId,
      productName:
        seed.productName,
      branchId:
        seed.branchId
        ?? this.defaultBranchId
    });

    if (seed.variantId) {

      this.seedVariant(
        0,
        seed.variantId
      );

      return;
    }

    this.seedProduct(
      0,
      seed.productId
    );
  }

  private seedProduct(
    index: number,
    productId: string
  ): void {

    const row =
      this.items.at(index);

    const branchId =
      row.value.branchId
      ?? this.defaultBranchId;

    if (!branchId) {
      return;
    }

    this.sellableService
      .search({
        branchId,
        search:
          row.value.productName
          ?? undefined,
        includePricing: true,
        includeAllocation: true,
        includeBatches: true
      })
      .pipe(
        takeUntilDestroyed(
          this.destroyRef
        )
      )
      .subscribe({

        next: response => {

          const variants =
            response.variants.content
              .filter(
                v =>
                  v.productId === productId
              );

          this.variantMap[index] =
            variants;

          if (variants.length !== 1) {
            return;
          }

          const variant =
            variants[0];

          this.packagingMap[index] =
            variant.packagings ?? [];

          row.patchValue({
            productVariantId:
              variant.variantId
          });

          if (
            variant.packagings?.length !== 1
          ) {
            return;
          }

          const packagingId =
            variant.packagings[0]
              .packagingId;

          const duplicateIndex =
            this.findDuplicateVariantPackaging(
              variant.variantId,
              packagingId,
              index
            );

          if (duplicateIndex !== -1) {

            this.removeLine(index);

            this.snackBar.open(
              `${row.value.productName} is already in the sale.`,
              'Close',
              {
                duration: 4000
              }
            );

            return;
          }

          row.patchValue({
            packagingId
          });
        }
      });
  }

  private seedVariant(
    index: number,
    variantId: string
  ): void {

    const row =
      this.items.at(index);

    const branchId =
      row.value.branchId
      ?? this.defaultBranchId;

    if (!branchId) {
      return;
    }

    console.log(
      'SEED VARIANT',
      variantId,
      branchId
    );

    row.patchValue({
      productVariantId:
        variantId
    });

    const request: SellableVariantRequest = {
      branchId,
      variantId,
      includePricing: true,
      includeAllocation: true,
      includeBatches: true,
      quantity: row.value.quantity ?? 1
    };

    this.sellableService
      .getVariant(request)
      .pipe(
        takeUntilDestroyed(
          this.destroyRef
        )
      )
      .subscribe({
        next: variant => {

          this.variantMap[index] = [
            variant
          ];

          this.packagingMap[index] =
            variant.packagings
            ?? [];

          row.patchValue({
            productId:
              variant.productId,

            productName:
              variant.productName,

            productVariantId:
              variant.variantId
          });

          if (
            variant.packagings
              ?.length === 1
          ) {
            row.patchValue({
              packagingId:
                variant.packagings[0]
                  .packagingId
            });
          }
        },

        error: () => {

          this.variantMap[index] =
            [];

          this.packagingMap[index] =
            [];

        }
      });
  }

  private setupCustomerLookup(): void {

    this.customerGroup.controls.phone.valueChanges
      .pipe(
        debounceTime(500),
        distinctUntilChanged(),
        filter(
          (phone): phone is string =>
            !!phone &&
            phone.length >= 9
        ),
        switchMap(phone =>
          this.customerService
            .lookupByPhone(phone)
        ),
        takeUntilDestroyed(this.destroyRef)
      )
      .subscribe({
        next: customer => {

          if (!customer) {
            this.unlockCustomerFields();
            return;
          }

          this.customerGroup.patchValue({
            name: customer.name,
            email: customer.emailAddresses?.[0] ?? null
          });

          this.lockCustomerFields();
        },
        error: () => {
          this.unlockCustomerFields();
        }
      });
  }

  addLine(): void {

    const row =
      this.createLineForm();

    this.items.push(row);

    const index =
      this.items.length - 1;

    this.variantMap[index] = [];

    this.packagingMap[index] = [];

    if (
      this.defaultBranchId
    ) {

      row.patchValue({

        branchId:
          this.defaultBranchId
      });
    }

    this.setupPreviewPipeline(index);
  }

  private createAdditionalRow(): number {

    const emptyIndex =
      this.items.controls.findIndex(
        c => !c.value.productId
      );

    if (emptyIndex !== -1) {
      return emptyIndex;
    }

    this.addLine();

    return this.items.length - 1;
  }

  private findDuplicateVariantPackaging(
    variantId: string,
    packagingId: string,
    excludeIndex: number
  ): number {

    return this.items.controls.findIndex(
      (control, index) => {

        if (index === excludeIndex) {
          return false;
        }

        const value =
          control.getRawValue();

        return (
          value.productVariantId === variantId
          &&
          value.packagingId === packagingId
        );
      }
    );
  }

  removeLine(index: number): void {

    this.items.removeAt(index);

    delete this.variantMap[index];
    delete this.packagingMap[index];
    delete this.previewStateMap[index];

    this.reindexStateMaps();
  }

  private reindexStateMaps(): void {

    this.variantMap =
      Object.values(this.variantMap)
        .reduce((acc, value, index) => {
          acc[index] = value;
          return acc;
        }, {} as Record<number, SellableVariantDTO[]>);

    this.packagingMap =
      Object.values(this.packagingMap)
        .reduce((acc, value, index) => {
          acc[index] = value;
          return acc;
        }, {} as Record<number, PackagingDTO[]>);

    this.previewStateMap =
      Object.values(this.previewStateMap)
        .reduce((acc, value, index) => {
          acc[index] = value;
          return acc;
        }, {} as Record<number, SaleLinePreviewState>);
  }

  private createLineForm(): SaleItemForm {

    return this.fb.group({
      productId: this.fb.control<string | null>(
        null,
        Validators.required
      ),
      productName: this.fb.control<string | null>(null),
      productVariantId: this.fb.control<string | null>(
        null,
        Validators.required
      ),
      packagingId: this.fb.control<string | null>(
        null,
        Validators.required
      ),
      branchId: this.fb.control<string | null>(
        this.defaultBranchId,
        Validators.required
      ),
      quantity: this.fb.control<number>(1, {
        nonNullable: true,
        validators: [
          Validators.required,
          Validators.min(1)
        ]
      }),
      batchSelections: this.fb.control<{
        batchId: string;
        quantity: number;
      }[] | null>(null),
      requestedUnitPrice: this.fb.control<number | null>(null),
      overrideReason: this.fb.control<string | null>(null)
    });
  }

  private setupPreviewPipeline(
    index: number
  ): void {

    const row =
      this.items.at(index);

    row.valueChanges
      .pipe(
        debounceTime(250),

        switchMap(() => {

          const value =
            row.getRawValue();

          if (
            !value.productVariantId ||
            !value.packagingId ||
            !value.branchId ||
            !value.quantity
          ) {

            this.previewStateMap[index] = {
              loading: false,
              resolved: false,
              preview: undefined,
              warnings: [],
              adjustments: [],
              allocations: []
            };

            return of(null);
          }

          const existing =
            this.previewStateMap[index];

          this.previewStateMap[index] = {
            loading: true,
            resolved: false,

            // IMPORTANT
            // preserve existing preview during refresh
            preview: existing?.preview,

            warnings: [],
            adjustments:
              existing?.adjustments ?? [],
            allocations:
              existing?.allocations ?? []
          };

          return this.previewService
            .previewLine({
              productVariantId:
                value.productVariantId,
              packagingId:
                value.packagingId,
              quantity:
                value.quantity,
              branchId:
                value.branchId,
              batchSelections:
                value.batchSelections ?? undefined,
              requestedUnitPrice:
                value.requestedUnitPrice ?? undefined,
              overrideReason:
                value.overrideReason ?? undefined,
            });
        }),

        takeUntilDestroyed(this.destroyRef)
      )
      .subscribe({

        next: preview => {

          if (!preview) {
            return;
          }

          this.previewStateMap[index] = {
            loading: false,
            resolved: true,
            preview,
            warnings: [],
            adjustments:
              preview.adjustments ?? [],
            allocations:
              preview.batchAllocations ?? []
          };
        },

        error: error => {

          const existing =
            this.previewStateMap[index];

          this.previewStateMap[index] = {
            loading: false,
            resolved: false,

            // preserve previous successful preview
            preview: existing?.preview,

            warnings: [
              error?.error?.message ??
              'Failed to resolve pricing and allocation.'
            ],

            adjustments:
              existing?.adjustments ?? [],

            allocations:
              existing?.allocations ?? []
          };
        }
      });
  }

  lockCustomerFields(): void {

    this.customerGroup.controls.name.disable();
    this.customerGroup.controls.email.disable();
  }

  unlockCustomerFields(): void {

    this.customerGroup.controls.name.enable();
    this.customerGroup.controls.email.enable();
  }

  openProductSelector(
    rowIndex: number
  ): void {

    this.dialog.open(
      ProductSelectorDialogComponent,
      {
        width: '90vw',
        maxWidth: '1200px',
        height: '90vh',
        panelClass: 'enterprise-dialog',
        data: {
          branchId:
            this.branchContext.currentBranch
            ?? this.defaultBranchId
        }
      }
    )
      .afterClosed()
      .subscribe((products: Product[]) => {

        if (!products?.length) {
          return;
        }

        products.forEach((product, index) => {

          const targetRow =
            index === 0
              ? rowIndex
              : this.createAdditionalRow();

          this.patchProductIntoRow(
            targetRow,
            product
          );
        });
      });
  }

  private patchProductIntoRow(
    index: number,
    product: Product
  ): void {

    const row =
      this.items.at(index);

    row.reset({
      productId: product.id,
      productName: product.name,
      productVariantId: null,
      packagingId: null,
      branchId:
        row.value.branchId ??
        this.defaultBranchId,
      quantity: 1,
      batchSelections: null,
      requestedUnitPrice: null,
      overrideReason: null
    });

    this.resolveSellables(
      index
    );

    this.seedProduct(
      index,
      product.id
    );
  }

  private resolveSellables(
    index: number
  ): void {

    const row =
      this.items.at(index);

    const value =
      row.getRawValue();

    if (!value.branchId) {
      return;
    }

    this.sellableService.search({
      branchId: value.branchId,
      search: value.productName ?? undefined,
      includePricing: true,
      includeAllocation: true,
      includeBatches: true,
      quantity: value.quantity
    })
      .pipe(
        takeUntilDestroyed(this.destroyRef)
      )
      .subscribe({

        next: response => {

          const variants =
            response.variants.content
              .filter(
                v =>
                  v.productId === value.productId
              );

          this.variantMap[index] =
            variants;

          if (variants.length !== 1) {
            return;
          }

          const variant = variants[0];

          row.patchValue({
            productVariantId:
              variant.variantId
          });

          this.packagingMap[index] =
            variant.packagings;

          if (
            variant.packagings?.length === 1
          ) {
            row.patchValue({
              packagingId:
                variant.packagings[0]
                  .packagingId
            });
          }
        },

        error: error => {

          this.snackBar.open(
            error?.error?.message ??
            'Unable to load product.',
            'Close',
            {
              duration: 5000
            }
          );
        }
      });
  }

  onVariantChange(
    index: number,
    variantId: string
  ): void {

    const variant =
      this.variantMap[index]
        ?.find(
          x =>
            x.variantId
            === variantId
        );

    this.packagingMap[
      index
    ] =
      variant?.packagings
      ?? [];

    this.items.at(index)
      .patchValue({

        packagingId:
          null
      });

    if (
      variant?.packagings
        ?.length === 1
    ) {

      this.items.at(index)
        .patchValue({

          packagingId:
            variant
              .packagings[0]
              .packagingId
        });
    }
  }

  linePreview(
    index: number
  ): SaleLinePreviewResponse | undefined {

    return this.previewStateMap[index]
      ?.preview;
  }

  lineTotal(
    index: number
  ): number {

    return this.linePreview(index)
      ?.totalPrice ?? 0;
  }

  grandTotal(): number {

    return Object.values(this.previewStateMap)
      .map(state =>
        state.preview?.totalPrice ?? 0
      )
      .reduce((a, b) => a + b, 0);
  }

  previewWarnings(
    index: number
  ): string[] {

    return this.previewStateMap[index]
      ?.warnings ?? [];
  }

  adjustments(
    index: number
  ): PricingAdjustment[] {

    return this.previewStateMap[index]
      ?.adjustments ?? [];
  }

  allocationPreview(
    index: number
  ): AllocationDetail[] {

    return this.previewStateMap[index]
      ?.allocations ?? [];
  }

  async startCameraScan(): Promise<void> {

    const code =
      await this.posBarcodeService
        .scanViaCamera();

    if (!code) {
      return;
    }

    this.scanBarcode(code);
  }

  scanBarcode(
    rawValue?: string
  ): void {

    const barcode =
      (rawValue ?? this.barcodeCtrl.value)
        ?.trim();

    if (!barcode) {
      return;
    }

    const branchId = this.branchContext.currentBranch ?? '';

    this.barcodeService.scan({
      branchId,
      barcode
    })
      .pipe(
        takeUntilDestroyed(this.destroyRef)
      )
      .subscribe({
        next: result => {

          const targetIndex =
            this.findReusableRow();

          const row =
            this.items.at(targetIndex);

          row.patchValue({
            productVariantId:
              result.variantId,
            packagingId:
              result.packagingId,
            branchId:
              result.branchId,
            quantity:
              result.requestedQuantity
          });

          this.barcodeCtrl.setValue('');
        },
        error: () => {
          this.snackBar.open(
            'Unable to resolve barcode.',
            'Close',
            {
              duration: 3000
            }
          );
        }
      });
  }

  private findReusableRow(): number {

    const emptyIndex =
      this.items.controls.findIndex(control =>
        !control.value.productId
      );

    if (emptyIndex !== -1) {
      return emptyIndex;
    }

    this.addLine();

    return this.items.length - 1;
  }

  openBatchAllocation(
    index: number
  ): void {

    const row =
      this.items.at(index)
        .getRawValue();

    if (
      !row.productVariantId ||
      !row.branchId ||
      !row.quantity
    ) {
      return;
    }

    this.dialog.open(
      BatchAllocationDialogComponent,
      {
        width: '1000px',
        maxWidth: '95vw',
        data: {
          variantId:
            row.productVariantId,
          branchId:
            row.branchId,
          quantity:
            row.quantity,
          existing:
            row.batchSelections
        }
      }
    )
      .afterClosed()
      .subscribe(result => {

        if (!result) {
          return;
        }

        this.items.at(index)
          .patchValue({
            batchSelections: result
          });
      });
  }

  controlAt(
    index: number,
    name: string
  ): FormControl {

    return this.items.at(index)
      .get(name) as FormControl;
  }

  canUseCameraScan(): boolean {

    return (
      'BarcodeDetector' in window &&
      navigator.mediaDevices &&
      typeof navigator.mediaDevices
        .getUserMedia === 'function'
    );
  }

  @HostListener(
    'document:keydown.control.enter'
  )
  submitShortcut(): void {

    if (this.form.valid) {
      this.submit();
    }
  }

  submit(): void {

    if (
      this.form.invalid ||
      this.submitting
    ) {
      return;
    }

    const unresolved =
      Object.values(this.previewStateMap)
        .some(state => !state.resolved);

    if (unresolved) {

      this.snackBar.open(
        'Wait for all pricing and allocation previews to resolve.',
        'Close',
        {
          duration: 4000
        }
      );

      return;
    }

    this.submitting = true;

    const customer =
      this.customerGroup.getRawValue();

    const payload: SaleRequest = {
      items: this.items.controls
        .map(control => {

          const value =
            control.getRawValue();

          return {
            productVariantId:
              value.productVariantId!,
            packagingId:
              value.packagingId!,
            branchId:
              value.branchId!,
            quantity:
              value.quantity,
            batchSelections:
              value.batchSelections ?? []
          };
        }),
      payments: [],
      totalAmount:
        this.grandTotal(),
      customerIdentifiers:
        customer.name ||
          customer.phone ||
          customer.email
          ? {
            name:
              customer.name ?? undefined,
            phone:
              customer.phone ?? undefined,
            email:
              customer.email ?? undefined
          }
          : undefined
    };

    this.salesService.create(payload)
      .pipe(
        takeUntilDestroyed(this.destroyRef)
      )
      .subscribe({
        next: sale => {

          this.submitting = false;

          this.router.navigate([
            '/app/sales',
            sale.id
          ]);
        },
        error: error => {

          this.submitting = false;

          this.snackBar.open(
            error?.error?.message ??
            'Failed to create sale.',
            'Close',
            {
              duration: 4000
            }
          );
        }
      });
  }
}