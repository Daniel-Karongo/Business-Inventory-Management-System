import { Component, Inject, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MAT_DIALOG_DATA, MatDialogRef, MatDialogModule } from '@angular/material/dialog';
import { FormBuilder, Validators, FormGroup } from '@angular/forms';
import { ReactiveFormsModule } from '@angular/forms';

import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar } from '@angular/material/snack-bar';

import { InventoryService } from '../../services/inventory.service';
import { SupplierService } from '../../../suppliers/services/supplier.service';
import { BranchService } from '../../../branches/services/branch.service';

import { SupplierMinimalDTO } from '../../../suppliers/models/supplier.model';
import { BranchMinimalDTO } from '../../../branches/models/branch.model';
import { Product } from '../../../products/parent/models/product.model';

@Component({
  selector: 'app-receive-new-product-dialog',
  standalone: true,
  imports: [
    CommonModule,
    MatDialogModule,
    ReactiveFormsModule,
    MatButtonModule,
    MatInputModule,
    MatSelectModule,
    MatFormFieldModule,
    MatIconModule
  ],
  templateUrl: './receive-new-product-dialog.component.html',
  styleUrls: ['./receive-new-product-dialog.component.scss']
})
export class ReceiveNewProductDialogComponent implements OnInit {

  loading = false;

  product!: Product;

  branches: BranchMinimalDTO[] = [];
  suppliers: SupplierMinimalDTO[] = [];

  form!: FormGroup;

  constructor(
    @Inject(MAT_DIALOG_DATA) public data: Product[],
    private dialogRef: MatDialogRef<ReceiveNewProductDialogComponent>,
    private fb: FormBuilder,
    private inventoryService: InventoryService,
    private supplierService: SupplierService,
    private branchService: BranchService,
    private snackbar: MatSnackBar
  ) { }

  ngOnInit(): void {
    // For now, handle one product at a time (explicit + safe)
    this.product = this.data[0];

    this.form = this.fb.group({
      branchId: ['', Validators.required],

      classification: ['', Validators.required],
      newVariantSku: [''],

      sellingPrice: [null],

      supplierId: ['', Validators.required],
      unitsSupplied: [1, [Validators.required, Validators.min(1)]],
      unitCost: [0, [Validators.required, Validators.min(0.01)]],

      reference: ['', Validators.required],
      note: ['']
    });

    this.loadBranches();
    this.loadSuppliers();
  }

  private loadBranches() {
    this.branchService.getAll(false).subscribe({
      next: branches => {
        this.branches = branches
          .filter(b => !!b.id)
          .map(b => ({
            id: b.id!,
            name: b.name,
            branchCode: b.branchCode ?? ''
          }));
      },
      error: () => {
        this.snackbar.open('Failed to load branches', 'Close', { duration: 3000 });
      }
    });
  }

  private loadSuppliers() {
    this.supplierService.getAll(false).subscribe({
      next: list => {
        this.suppliers = list
          .filter(s => !!s.id)
          .map(s => ({ id: s.id!, name: s.name }));
      },
      error: () => {
        this.snackbar.open('Failed to load suppliers', 'Close', { duration: 3000 });
      }
    });
  }

  close() {
    this.dialogRef.close();
  }

  submit() {
    if (this.form.invalid) return;

    this.loading = true;

    const payload = {
      productId: this.product.id,
      branchId: this.form.value.branchId,

      // 👇 this is the KEY: new variant creation
      classification: this.form.value.classification,
      newVariantSku: this.form.value.newVariantSku,

      sellingPrice: this.form.value.sellingPrice,

      reference: this.form.value.reference,
      note: this.form.value.note,

      suppliers: [{
        supplierId: this.form.value.supplierId,
        unitsSupplied: this.form.value.unitsSupplied,
        unitCost: this.form.value.unitCost
      }]
    };

    this.inventoryService.receiveStock(payload).subscribe({
      next: () => {
        this.snackbar.open('Product received successfully', 'Close', { duration: 2500 });
        this.dialogRef.close(true);
      },
      error: () => {
        this.loading = false;
        this.snackbar.open('Failed to receive product', 'Close', { duration: 3000 });
      }
    });
  }
}