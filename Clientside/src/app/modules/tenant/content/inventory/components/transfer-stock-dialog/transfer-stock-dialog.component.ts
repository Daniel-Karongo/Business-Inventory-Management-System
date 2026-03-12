import { CommonModule } from '@angular/common';
import { Component, Inject } from '@angular/core';
import { FormBuilder, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatButtonModule } from '@angular/material/button';
import { MAT_DIALOG_DATA, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { BranchMinimalDTO } from '../../../branches/models/branch.model';
import { InventoryService } from '../../services/inventory.service';
import { BranchService } from '../../../branches/services/branch.service';
import { MatSnackBar } from '@angular/material/snack-bar';

@Component({
  standalone: true,
  selector: 'app-transfer-stock-dialog',
  imports: [
    CommonModule,
    MatDialogModule,
    MatButtonModule,
    MatInputModule,
    MatFormFieldModule,
    MatSelectModule,
    ReactiveFormsModule
  ],
  templateUrl: './transfer-stock-dialog.component.html',
  styleUrls: ['./transfer-stock-dialog.component.scss']
})
export class TransferStockDialogComponent {

  loading = false;
  form!: FormGroup;
  branches: BranchMinimalDTO[] = [];

  constructor(
    @Inject(MAT_DIALOG_DATA) public data: any,
    private dialogRef: MatDialogRef<TransferStockDialogComponent>,
    private fb: FormBuilder,
    private inventoryService: InventoryService,
    private branchService: BranchService,
    private snackbar: MatSnackBar
  ) { }

  ngOnInit() {
    this.form = this.fb.group({
      toBranchId: ['', Validators.required],
      quantity: [1, [Validators.required, Validators.min(1)]],
      destinationUnitCost: [
        this.data.averageCost ?? null
      ],
      reference: ['', Validators.required],
      note: ['']
    });

    this.branchService.getAll(false).subscribe(list => {
      this.branches = list
        .filter(b => !!b.id && b.id !== this.data.fromBranchId)
        .map(b => ({
          id: b.id!,
          name: b.name,
          branchCode: b.branchCode ?? ''
        }));

      // 🔐 EDGE-CASE GUARD — PUT IT HERE
      if (this.branches.length === 0) {
        this.snackbar.open(
          'No other branches available for transfer',
          'Close',
          { duration: 3000 }
        );
        this.dialogRef.close();
      }
    });
  }

  submit() {
    if (this.form.invalid) return;

    if (this.form.value.quantity > this.data.available) {
      this.snackbar.open('Quantity exceeds available stock', 'Close', { duration: 3000 });
      return;
    }

    this.loading = true;

    const payload = {
      productVariantId: this.data.productVariantId,
      fromBranchId: this.data.fromBranchId,
      ...this.form.value
    };

    this.inventoryService.transferStock(payload).subscribe({
      next: () => {
        this.snackbar.open('Stock transferred successfully', 'Close', { duration: 2500 });
        this.dialogRef.close(true);
      },
      error: () => {
        this.loading = false;
        this.snackbar.open('Stock transfer failed', 'Close', { duration: 3000 });
      }
    });
  }

  close() {
    this.dialogRef.close();
  }
}