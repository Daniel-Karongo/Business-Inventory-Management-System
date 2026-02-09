import { Component, HostListener, OnInit } from '@angular/core';
import { CommonModule, CurrencyPipe } from '@angular/common';
import { FormArray, FormBuilder, FormControl, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms';
import { Router } from '@angular/router';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatTableModule } from '@angular/material/table';

import { ProductService } from '../../../products/parent/services/product.service';
import { ProductVariantService } from '../../../products/variant/services/product-variant.service';
import { BranchService } from '../../../branches/services/branch.service';
import { SalesService } from '../../services/sales.service';
import { InventoryService } from '../../../inventory/services/inventory.service';
import { CustomerService } from '../../../customers/services/customer.service';
import { catchError, debounceTime, distinctUntilChanged, filter, of, switchMap } from 'rxjs';
import { AuthService } from '../../../auth/services/auth.service';
import { ViewChild, ElementRef } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../../environments/environment';
import { MatDialog } from '@angular/material/dialog';
import { ProductSelectorDialogComponent } from '../../dialogs/product-selector-dialog/product-selector-dialog.component';
import { Product } from '../../../products/parent/models/product.model';
import { MatTooltipModule } from '@angular/material/tooltip';

type SaleItemForm = FormGroup<{
  productId: FormControl<string | null>;
  productName: FormControl<string | null>; // ✅ ADD
  productVariantId: FormControl<string | null>;
  branchId: FormControl<string | null>;
  quantity: FormControl<number>;
  unitPrice: FormControl<number | null>;
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
    MatTableModule,
    MatTooltipModule
  ],
  templateUrl: './sale-create.component.html',
  styleUrls: ['./sale-create.component.scss']
})
export class SaleCreateComponent implements OnInit {
  @ViewChild('barcodeInput') barcodeInput!: ElementRef<HTMLInputElement>;

  scanning = false;
  barcodeValue = '';
  barcodeCtrl = new FormControl('');

  form!: FormGroup<{
    customer: FormGroup<{
      name: FormControl<string | null>;
      phone: FormControl<string | null>;
      email: FormControl<string | null>;
    }>;
    items: FormArray<SaleItemForm>;
  }>;

  products: any[] = [];
  variants: any[] = [];
  branches: any[] = [];

  displayedColumns = ['product', 'branch', 'qty', 'price', 'total', 'remove'];
  variantsMap: Record<number, any[] | undefined> = {};
  stockMap: Record<number, number | undefined> = {};

  defaultBranchId: string | null = null;

  constructor(
    private fb: FormBuilder,
    private productService: ProductService,
    private variantService: ProductVariantService,
    private inventoryService: InventoryService,
    private branchService: BranchService,
    private salesService: SalesService,
    private customerService: CustomerService,
    private router: Router,
    private authService: AuthService,
    private http: HttpClient,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {
    this.form = this.fb.group({
      customer: this.fb.group({
        name: this.fb.control<string | null>(null),
        phone: this.fb.control<string | null>(null),
        email: this.fb.control<string | null>(null),
      }),
      items: this.fb.array<FormGroup>([], Validators.required),
    });

    const me = this.authService.getSnapshot();
    this.defaultBranchId = me?.branchId ?? null;

    this.form.controls.customer.controls.phone.valueChanges
      .pipe(
        debounceTime(500),
        distinctUntilChanged(),
        filter((phone): phone is string => !!phone && phone.length >= 9),
        switchMap(phone =>
          this.customerService.lookupByPhone(phone).pipe(
            catchError(() => of(null))
          )
        )
      )
      .subscribe(customer => {
        if (!customer) {
          this.unlockCustomerFields();
          return;
        }

        this.form.controls.customer.patchValue({
          name: customer.name,
          email: customer.emailAddresses?.[0] ?? null
        });

        this.lockCustomerFields();
      });

    this.productService.getAll().subscribe(p => this.products = p);
    this.branchService.getAll(false).subscribe(b => this.branches = b);

    this.addLine();
    this.variantsMap[0] = [];
  }

  private createSaleItemForm(
    init?: Partial<{
      productId: string;
      productName: string;
      productVariantId: string;
      branchId: string;
      quantity: number;
      unitPrice: number;
    }>
  ): SaleItemForm {
    return this.fb.group({
      productId: this.fb.control<string | null>(init?.productId ?? null, Validators.required),
      productName: this.fb.control<string | null>(init?.productName ?? null),
      productVariantId: this.fb.control<string | null>(init?.productVariantId ?? null, Validators.required),
      branchId: this.fb.control<string | null>(init?.branchId ?? this.defaultBranchId, Validators.required),
      quantity: this.fb.control<number>(init?.quantity ?? 1, {
        nonNullable: true,
        validators: [Validators.required, Validators.min(1)]
      }),
      unitPrice: this.fb.control<number | null>(init?.unitPrice ?? null)
    });
  }

  get customerGroup(): FormGroup {
    return this.form.controls.customer;
  }

  get items(): FormArray<SaleItemForm> {
    return this.form.controls.items;
  }

  lockCustomerFields() {
    this.form.controls.customer.controls.name.disable();
    this.form.controls.customer.controls.email.disable();
  }

  unlockCustomerFields() {
    this.form.controls.customer.controls.name.enable();
    this.form.controls.customer.controls.email.enable();
  }

  addLine() {
    this.items.push(this.createSaleItemForm());

    const index = this.items.length - 1;
    this.variantsMap[index] = [];   // ✅ GUARANTEE
  }

  removeLine(i: number) {
  this.items.removeAt(i);

  delete this.variantsMap[i];
  delete this.stockMap[i];

  // 🔁 reindex variantsMap
  this.variantsMap = Object.values(this.variantsMap).reduce(
    (acc, v, idx) => {
      acc[idx] = v;
      return acc;
    },
    {} as Record<number, any[] | undefined>
  );

  // 🔁 reindex stockMap
  this.stockMap = Object.values(this.stockMap).reduce(
    (acc, v, idx) => {
      acc[idx] = v;
      return acc;
    },
    {} as Record<number, number | undefined>
  );
}


  scanBarcode(value?: string) {
    const code = (value ?? this.barcodeCtrl.value)?.trim();
    if (!code) return;

    const payload = {
      barcode: code,
      branchId: this.defaultBranchId
    };

    this.http.post<any>(
      `${environment.apiUrl}/product-variants/scan`,
      payload
    ).subscribe({
      next: res => this.applyScannedVariant(res),
      error: () => alert('Invalid barcode / SKU')
    });

    this.barcodeCtrl.setValue('');
    // setTimeout(() => this.barcodeInput?.nativeElement.focus());
  }

  private applyScannedVariant(res: any) {
    const branchId = res.branchId ?? this.defaultBranchId;

    /* 1️⃣ Exact match: same variant + branch → increment qty */
    const exactIndex = this.items.controls.findIndex(ctrl =>
      ctrl.value.productVariantId === res.variantId &&
      ctrl.value.branchId === branchId
    );

    if (exactIndex !== -1) {
      const qtyCtrl = this.items.at(exactIndex).get('quantity')!;
      qtyCtrl.setValue(qtyCtrl.value + 1);
      return;
    }

    /* 2️⃣ Same product exists but variant not yet selected */
    const productOnlyIndex = this.items.controls.findIndex(ctrl =>
      ctrl.value.productId === res.productId &&
      !ctrl.value.productVariantId
    );

    if (productOnlyIndex !== -1) {
      const row = this.items.at(productOnlyIndex);

      row.patchValue({
        productVariantId: res.variantId,
        unitPrice: res.sellingPrice
      });

      this.ensureVariantsLoaded(
        productOnlyIndex,
        res.productId,
        res.variantId
      );

      const qtyCtrl = row.get('quantity')!;
      qtyCtrl.setValue(qtyCtrl.value + 1);
      return;
    }

    /* 3️⃣ Empty row (no product yet) */
    const emptyIndex = this.items.controls.findIndex(ctrl =>
      !ctrl.value.productId
    );

    if (emptyIndex !== -1) {
      const row = this.items.at(emptyIndex);

      row.patchValue({
        productId: res.productId,
        productName: res.productName,
        productVariantId: res.variantId,
        branchId,
        quantity: 1,
        unitPrice: res.sellingPrice
      });

      this.ensureVariantsLoaded(
        emptyIndex,
        res.productId,
        res.variantId
      );

      if (branchId) {
        this.stockMap[emptyIndex] = res.quantityOnHand ?? 0;
      }

      return;
    }

    /* 4️⃣ Otherwise → create new row */
    const row = this.createSaleItemForm({
      productId: res.productId,
      productName: res.productName,
      productVariantId: res.variantId,
      branchId,
      quantity: 1,
      unitPrice: res.sellingPrice
    });

    this.items.push(row);
    const i = this.items.length - 1;

    this.ensureVariantsLoaded(
      i,
      res.productId,
      res.variantId
    );

    if (branchId) {
      this.stockMap[i] = res.quantityOnHand ?? 0;
    }
  }

  private clearVariantSelection(index: number) {
    const row = this.items.at(index);

    row.patchValue({
      productVariantId: null,
      unitPrice: null
    });

    delete this.stockMap[index];
  }

  private clearProductVariants(index: number) {
    this.variantsMap[index] = [];
  }

  private resetProductDependentState(index: number) {
    const row = this.items.at(index);

    row.patchValue({
      productVariantId: null,
      unitPrice: null
    });

    delete this.stockMap[index];
    this.variantsMap[index] = [];
  }


  canUseCameraScan(): boolean {
    return (
      'BarcodeDetector' in window &&
      navigator.mediaDevices &&
      typeof navigator.mediaDevices.getUserMedia === 'function'
    );
  }

  async startCameraScan() {
    if (!('BarcodeDetector' in window)) {
      alert('Camera scanning not supported on this device');
      return;
    }

    const detector = new (window as any).BarcodeDetector({
      formats: ['code_128', 'ean_13', 'ean_8']
    });

    const stream = await navigator.mediaDevices.getUserMedia({
      video: { facingMode: 'environment' }
    });

    const video = document.createElement('video');
    video.srcObject = stream;
    video.play();

    this.scanning = true;

    const scanLoop = async () => {
      if (!this.scanning) return;

      const barcodes = await detector.detect(video);
      if (barcodes.length) {
        this.scanning = false;
        stream.getTracks().forEach(t => t.stop());
        this.scanBarcode(barcodes[0].rawValue);
        return;
      }

      requestAnimationFrame(scanLoop);
    };

    scanLoop();
  }

  openProductSelector(rowIndex: number) {
    this.dialog.open(ProductSelectorDialogComponent, {
      width: '1000px',
      maxHeight: '85vh'
    }).afterClosed().subscribe((products: Product[]) => {
      if (!products?.length) return;

      products.forEach(p => {

        /* 1️⃣ Check if product already exists in any row */
        const existingIndex = this.items.controls.findIndex(ctrl =>
          ctrl.value.productId === p.id
        );

        if (existingIndex !== -1) {
          // ✅ Increment quantity instead of adding row
          const qtyCtrl = this.items.at(existingIndex).get('quantity')!;
          qtyCtrl.setValue(qtyCtrl.value + 1);
          return;
        }

        /* 2️⃣ Use current row if empty */
        const targetRow =
          !this.items.at(rowIndex).value.productId
            ? rowIndex
            : this.items.controls.findIndex(ctrl => !ctrl.value.productId);

        if (targetRow !== -1) {
          this.patchProductIntoRow(targetRow, p);
          return;
        }

        /* 3️⃣ Otherwise create new row */
        this.addProductAsNewLine(p);
      });
    });
  }

  private patchProductIntoRow(i: number, p: Product) {
    const row = this.items.at(i);

    row.patchValue({
      productId: p.id,
      productName: p.name
    });

    this.clearVariantSelection(i);
    this.clearProductVariants(i); // ✅ OK here

    this.variantService.forProduct(p.id).subscribe(v => {
      this.variantsMap[i] = v;
      if (v.length === 1) {
        row.patchValue({
          productVariantId: v[0].id,
          unitPrice: v[0].minimumSellingPrice
        });
      }
    });
  }

  private addProductAsNewLine(p: Product) {
    const row = this.createSaleItemForm({
      productId: p.id,
      productName: p.name
    });

    this.items.push(row);
    const i = this.items.length - 1;

    this.variantService.forProduct(p.id).subscribe(v => {
      this.variantsMap[i] = v;
      if (v.length === 1) {
        row.patchValue({
          productVariantId: v[0].id,
          unitPrice: v[0].minimumSellingPrice
        });
      }
    });
  }

  private ensureVariantsLoaded(
    index: number,
    productId: string,
    selectedVariantId?: string
  ) {
    // Prevent duplicate loads
    if (this.variantsMap[index]?.length) return;

    this.variantService.forProduct(productId).subscribe(variants => {
      this.variantsMap[index] = variants;

      // Keep scanned variant selected
      if (selectedVariantId && variants.some(v => v.id === selectedVariantId)) {
        this.items.at(index).patchValue({
          productVariantId: selectedVariantId
        });
      }

      // Auto-select if only one variant
      if (variants.length === 1) {
        this.items.at(index).patchValue({
          productVariantId: variants[0].id,
          unitPrice: variants[0].minimumSellingPrice
        });
      }
    });
  }

  onProductChange(i: number) {
    const row = this.items.at(i);
    if (!row) return;

    const productId = row.get('productId')?.value;
    if (!productId) return;

    // 🔥 RESET dependent fields
    row.patchValue({
      productVariantId: null,
      unitPrice: null
    });

    delete this.stockMap[i];
    this.variantsMap[i] = [];

    // Load variants for selected product
    this.variantService.forProduct(productId)
      .subscribe(v => {
        this.variantsMap[i] = v;

        // ✅ Auto-select variant if only one
        if (v.length === 1) {
          row.get('productVariantId')?.setValue(v[0].id);
          this.onVariantChange(i, v[0].id);
        }
      });
  }

  onVariantChange(i: number, variantId: string) {
    const row = this.items.at(i);
    if (!row) return;

    const variant = this.variantsMap[i]?.find(v => v.id === variantId);
    if (!variant) return;

    // ✅ auto-fill unit price
    row.get('unitPrice')?.setValue(variant.minimumSellingPrice);

    const branchId = row.get('branchId')?.value;
    if (branchId) {
      this.loadStock(i, variant.id, branchId);
    }
  }

  loadStock(i: number, variantId: string, branchId: string) {
    this.inventoryService.getVariantStock(variantId, branchId)
      .subscribe(stock => {

        if (!stock) return;

        const available =
          stock.quantityOnHand - stock.quantityReserved;

        this.stockMap[i] = available;

        const row = this.items.at(i);
        const qtyCtrl = row.get('quantity');

        if (!qtyCtrl) return;

        if (qtyCtrl.value > available) {
          qtyCtrl.setErrors({ stockExceeded: true });
        } else {
          if (qtyCtrl.hasError('stockExceeded')) {
            const errors = { ...qtyCtrl.errors };
            delete errors['stockExceeded'];
            qtyCtrl.setErrors(Object.keys(errors).length ? errors : null);
          }
        }
      });
  }

  lineTotal(i: number): number {
    const row = this.items.at(i).value;
    const qty = Number(row.quantity || 0);
    const price = Number(row.unitPrice || 0);
    return qty * price;
  }

  grandTotal(): number {
    return this.items.controls
      .map((_, i) => this.lineTotal(i))
      .reduce((a, b) => a + b, 0);
  }

  controlAt(index: number, name: string): FormControl {
    return this.items.at(index).get(name) as FormControl;
  }

  @HostListener('document:keydown.control.enter')
  submitShortcut() {
    if (this.form.valid) this.submit();
  }

  submit() {
    if (this.form.invalid) return;

    const customer = this.form.controls.customer.value;
    const items = this.items.controls.map(ctrl => ctrl.value);

    const payload: any = {
      items: items.map(r => ({
        productVariantId: r.productVariantId!,
        branchId: r.branchId!,
        quantity: r.quantity!,
        unitPrice: r.unitPrice!
      }))
    };

    // ✅ Attach customer only if at least one identifier exists
    if (customer.name || customer.phone || customer.email) {
      payload.customerIdentifiers = {
        name: customer.name ?? null,
        phone: customer.phone ?? null,
        email: customer.email ?? null
      };
    }

    this.salesService.create(payload).subscribe({
      next: sale => this.router.navigate(['/sales', sale.id])
    });
  }
}