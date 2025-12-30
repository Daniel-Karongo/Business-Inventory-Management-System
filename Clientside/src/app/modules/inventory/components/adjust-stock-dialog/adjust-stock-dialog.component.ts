import { Component, Inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MAT_DIALOG_DATA, MatDialogRef, MatDialogModule } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { ReactiveFormsModule, FormBuilder, Validators, FormGroup } from '@angular/forms';
import { MatSnackBar } from '@angular/material/snack-bar';

import { InventoryService } from '../../services/inventory.service';

@Component({
  selector: 'app-adjust-stock-dialog',
  standalone: true,
  imports: [
    CommonModule,
    MatDialogModule,
    MatButtonModule,
    MatInputModule,
    ReactiveFormsModule
  ],
  templateUrl: './adjust-stock-dialog.component.html',
  styleUrls: ['./adjust-stock-dialog.component.scss']
})
export class AdjustStockDialogComponent {

  loading = false;
  form!: FormGroup;


  constructor(
    @Inject(MAT_DIALOG_DATA) public data: any,
    private dialogRef: MatDialogRef<AdjustStockDialogComponent>,
    private inventoryService: InventoryService,
    private snackbar: MatSnackBar,
    private fb: FormBuilder
  ) { }

  ngOnInit() {
    this.form = this.fb.group({
      quantityDelta: [0, Validators.required],
      reason: ['', Validators.required],
      reference: ['', Validators.required]
    });
  }

  close() {
    this.dialogRef.close();
  }

  submit() {
    if (this.form.invalid) return;

    this.loading = true;

    const payload = {
      productVariantId: this.data.productVariantId,
      branchId: this.data.branchId,
      ...this.form.value
    };

    this.inventoryService.adjustVariantStock(payload).subscribe({
      next: () => {
        this.snackbar.open('Stock adjusted successfully', 'Close', { duration: 2500 });
        this.dialogRef.close(true);
      },
      error: () => {
        this.loading = false;
        this.snackbar.open('Stock adjustment failed', 'Close', { duration: 3000 });
      }
    });
  }
}