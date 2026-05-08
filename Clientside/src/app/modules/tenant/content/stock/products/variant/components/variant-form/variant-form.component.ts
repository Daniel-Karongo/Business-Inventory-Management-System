import {
  Component,
  Inject,
  OnInit
} from '@angular/core';

import {
  CommonModule
} from '@angular/common';

import {
  FormBuilder,
  FormGroup,
  ReactiveFormsModule
} from '@angular/forms';

import {
  MAT_DIALOG_DATA,
  MatDialogModule,
  MatDialogRef
} from '@angular/material/dialog';

import {
  MatButtonModule
} from '@angular/material/button';

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
  ProductVariant
} from '../../../../models/product-variant.model';

import {
  ProductVariantService
} from '../../services/product-variant.service';

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
  templateUrl:
    './variant-form.component.html',
  styleUrls: [
    './variant-form.component.scss'
  ]
})
export class VariantFormComponent
  implements OnInit {

  saving = false;

  form!: FormGroup;

  constructor(
    private readonly fb: FormBuilder,
    private readonly dialogRef:
      MatDialogRef<VariantFormComponent>,
    private readonly variantService:
      ProductVariantService,
    @Inject(MAT_DIALOG_DATA)
    public readonly variant:
      ProductVariant
  ) { }

  ngOnInit(): void {

    this.form = this.fb.group({
      classification: [
        {
          value:
            this.variant.classification,
          disabled: true
        }
      ],

      sku: [
        this.variant.sku ?? null
      ],

      minimumPercentageProfit: [
        this.variant.minimumPercentageProfit ?? null
      ],

      minimumProfit: [
        this.variant.minimumProfit ?? null
      ]
    });
  }

  save(): void {

    if (
      this.form.invalid ||
      this.saving
    ) {
      return;
    }

    this.saving = true;

    const raw =
      this.form.getRawValue();

    this.variantService.update(
      this.variant.id,
      {
        sku:
          raw.sku ?? undefined,

        minimumPercentageProfit:
          raw.minimumPercentageProfit ?? undefined,

        minimumProfit:
          raw.minimumProfit ?? undefined
      }
    )
      .subscribe({
        next: updated =>
          this.dialogRef.close(updated),

        error: () => {
          this.saving = false;
        }
      });
  }

  cancel(): void {

    this.dialogRef.close();
  }
}