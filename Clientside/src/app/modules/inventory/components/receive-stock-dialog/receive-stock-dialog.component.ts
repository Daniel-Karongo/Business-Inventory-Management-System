import { Component, Inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MAT_DIALOG_DATA, MatDialogRef, MatDialogModule } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatSnackBar } from '@angular/material/snack-bar';
import { ReactiveFormsModule, FormBuilder, Validators, FormArray, FormGroup } from '@angular/forms';

import { InventoryService } from '../../services/inventory.service';
import { MatIconModule } from '@angular/material/icon';
import { SupplierMinimalDTO } from '../../../suppliers/models/supplier.model';
import { SupplierService } from '../../../suppliers/services/supplier.service';
import { MatOption, MatSelectModule } from '@angular/material/select';
import { MatFormFieldModule } from '@angular/material/form-field';

@Component({
  selector: 'app-receive-stock-dialog',
  standalone: true,
  imports: [
    CommonModule,
    MatDialogModule,
    MatButtonModule,
    MatInputModule,
    MatFormFieldModule,
    MatSelectModule,
    ReactiveFormsModule,
    MatIconModule
  ],
  templateUrl: './receive-stock-dialog.component.html',
  styleUrls: ['./receive-stock-dialog.component.scss']
})
export class ReceiveStockDialogComponent {

  loading = false;
  form!: FormGroup;
  suppliersList: SupplierMinimalDTO[] = [];

  constructor(
    @Inject(MAT_DIALOG_DATA) public data: any,
    private dialogRef: MatDialogRef<ReceiveStockDialogComponent>,
    private inventoryService: InventoryService,
    private snackbar: MatSnackBar,
    private fb: FormBuilder,
    private supplierService: SupplierService
  ) { }

  ngOnInit() {
    this.form = this.fb.group({
      reference: ['', Validators.required],
      note: [''],
      suppliers: this.fb.array([this.createSupplier()])
    });

    this.loadSuppliers();
  }

  loadSuppliers() {
    this.supplierService.getAll(false).subscribe({
      next: list => {
        this.suppliersList = list
          .filter(s => !!s.id)
          .map(s => ({ id: s.id!, name: s.name }));
      }
    });
  }

  get suppliers(): FormArray {
    return this.form.get('suppliers') as FormArray;
  }

  createSupplier() {
    return this.fb.group({
      supplierId: ['', Validators.required],
      unitsSupplied: [0, [Validators.required, Validators.min(1)]],
      unitCost: [0, [Validators.required, Validators.min(0.01)]]
    });
  }

  addSupplier() {
    this.suppliers.push(this.createSupplier());
  }

  removeSupplier(index: number) {
    this.suppliers.removeAt(index);
  }

  close() {
    this.dialogRef.close();
  }

  submit() {
    if (this.form.invalid) return;

    this.loading = true;

    const payload = {
      productId: this.data.productId,
      productVariantId: this.data.productVariantId,
      classification: this.data.classification,
      branchId: this.data.branchId,
      ...this.form.value
    };

    this.inventoryService.receiveStock(payload).subscribe({
      next: () => {
        this.snackbar.open('Stock received successfully', 'Close', { duration: 2500 });
        this.dialogRef.close(true);
      },
      error: () => {
        this.loading = false;
        this.snackbar.open('Failed to receive stock', 'Close', { duration: 3000 });
      }
    });
  }
}