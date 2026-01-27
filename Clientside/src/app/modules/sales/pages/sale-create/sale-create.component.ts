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
    MatTableModule
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
    items: FormArray<FormGroup>;
  }>;

  products: any[] = [];
  variants: any[] = [];
  branches: any[] = [];
  variantsMap: Record<number, any[]> = {};


  displayedColumns = ['product', 'branch', 'qty', 'price', 'total', 'remove'];
  stockMap: Record<number, number> = {};
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
    private http: HttpClient
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
  }

  get customerGroup(): FormGroup {
    return this.form.controls.customer;
  }

  get items(): FormArray {
    return this.form.get('items') as FormArray;
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
    const row = this.fb.group({
      productId: [null, Validators.required],
      productVariantId: [null, Validators.required],
      branchId: [
        this.defaultBranchId,              // ✅ DEFAULT HERE
        Validators.required
      ],
      quantity: [1, [Validators.required, Validators.min(1)]],
      unitPrice: [null]
    });

    row.get('quantity')!.valueChanges.subscribe(qty => {
      const i = this.items.controls.indexOf(row);
      const available = this.stockMap[i];
      if (available == null) return;

      const q = Number(qty ?? 0);
      const ctrl = row.get('quantity')!;

      if (q > available) {
        ctrl.setErrors({ ...(ctrl.errors ?? {}), stockExceeded: true });
      } else if (ctrl.hasError('stockExceeded')) {
        const errors = { ...(ctrl.errors ?? {}) };
        delete errors['stockExceeded'];
        ctrl.setErrors(Object.keys(errors).length ? errors : null);
      }
    });

    this.items.push(row);
  }

  removeLine(i: number) {
    this.items.removeAt(i);
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
  }

  private applyScannedVariant(res: any) {

    /* 1️⃣ If variant already exists → increment qty */
    const existingIndex = this.items.controls.findIndex(ctrl =>
      ctrl.value.productVariantId === res.variantId &&
      ctrl.value.branchId === (res.branchId ?? this.defaultBranchId)
    );

    if (existingIndex !== -1) {
      const qtyCtrl = this.items.at(existingIndex).get('quantity')!;
      qtyCtrl.setValue(Number(qtyCtrl.value) + 1);
      return;
    }

    /* 2️⃣ Find first EMPTY line (no variant yet) */
    const emptyIndex = this.items.controls.findIndex(ctrl =>
      !ctrl.value.productVariantId
    );

    const targetRow =
      emptyIndex !== -1
        ? this.items.at(emptyIndex)
        : null;

    if (targetRow) {
      targetRow.patchValue({
        productId: res.productId,
        productVariantId: res.variantId,
        branchId: res.branchId ?? this.defaultBranchId,
        quantity: 1,
        unitPrice: res.sellingPrice
      });

      if (res.branchId) {
        this.stockMap[emptyIndex] = res.quantityOnHand ?? 0;
      }

      return;
    }

    /* 3️⃣ Otherwise → create new line */
    const row = this.fb.group({
      productId: [res.productId, Validators.required],
      productVariantId: [res.variantId, Validators.required],
      branchId: [res.branchId ?? this.defaultBranchId, Validators.required],
      quantity: [1, [Validators.required, Validators.min(1)]],
      unitPrice: [res.sellingPrice, Validators.required]
    });

    this.items.push(row);

    if (res.branchId) {
      this.stockMap[this.items.length - 1] = res.quantityOnHand ?? 0;
    }
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
      .subscribe(res => {
        const available =
          res.quantityOnHand - res.quantityReserved;

        this.stockMap[i] = available;

        const row = this.items.at(i);
        const qtyCtrl = row.get('quantity');

        if (!qtyCtrl) return;

        if (qtyCtrl.value > available) {
          qtyCtrl.setErrors({ stockExceeded: true });
        } else {
          // IMPORTANT: clear only this error, not others
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