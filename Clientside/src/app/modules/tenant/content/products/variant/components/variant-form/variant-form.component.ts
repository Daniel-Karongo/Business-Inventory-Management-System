import { Component, Inject, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormBuilder, FormGroup, ReactiveFormsModule } from '@angular/forms';

import { MatDialogRef, MAT_DIALOG_DATA, MatDialogModule } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { MatFormFieldModule } from '@angular/material/form-field';

import { ProductVariant } from '../../models/product-variant.model';
import { ProductVariantService } from '../../services/product-variant.service';

@Component({
  selector: 'app-variant-form',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatDialogModule,
    MatButtonModule,
    MatInputModule,
    MatIconModule,
    MatFormFieldModule
  ],
  templateUrl: './variant-form.component.html',
  styleUrls: ['./variant-form.component.scss']
})
export class VariantFormComponent implements OnInit {

  saving = false;
  form!: FormGroup;

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<VariantFormComponent>,
    private variantService: ProductVariantService,
    @Inject(MAT_DIALOG_DATA) public variant: ProductVariant
  ) {}

  ngOnInit(): void {
    // ✅ form MUST be built here
    this.form = this.fb.group({
      classification: [{ value: this.variant.classification, disabled: true }],
      sku: [this.variant.sku ?? null],
      minimumSellingPrice: [this.variant.minimumSellingPrice ?? null],
      averageBuyingPrice: [this.variant.averageBuyingPrice ?? null]
    });
  }

  save() {
    if (this.form.invalid || this.saving) return;

    this.saving = true;

    const raw = this.form.getRawValue();

    const payload: Partial<ProductVariant> = {
      sku: raw.sku ?? undefined,
      minimumSellingPrice: raw.minimumSellingPrice ?? undefined,
      averageBuyingPrice: raw.averageBuyingPrice ?? undefined
    };

    this.variantService.update(this.variant.id, payload).subscribe({
      next: updated => this.dialogRef.close(updated),
      error: () => this.saving = false
    });
  }

  cancel() {
    this.dialogRef.close();
  }
}