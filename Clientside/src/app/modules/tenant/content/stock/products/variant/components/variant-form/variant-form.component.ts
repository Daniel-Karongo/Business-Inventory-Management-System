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
  MatDialog,
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
import { ReasonDialogComponent } from '../../../../../../../../shared/components/reason-dialog/reason-dialog.component';

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
    private readonly dialogRef: MatDialogRef<VariantFormComponent>,
    private readonly variantService: ProductVariantService,
    @Inject(MAT_DIALOG_DATA)
    public readonly variant: ProductVariant,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {

    if (!this.variant) {

      console.error(
        'VariantFormComponent received undefined variant'
      );

      this.form = this.fb.group({
        classification: [''],
        sku: [''],
        minimumPercentageProfit: [null],
        minimumProfit: [null]
      });

      return;
    }

    this.form = this.fb.group({
      classification: [
        this.variant.classification
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
    if (this.form.invalid || this.saving) {
      return;
    }

    const dialogRef = this.dialog.open(
      ReasonDialogComponent,
      {
        width: '500px',
        data: {
          title: 'Update Variant',
          message:
            'Provide a reason for this update.',
          action: 'RESTORE',
          requireReason: false
        }
      }
    );

    dialogRef.afterClosed().subscribe(result => {
      if (!result?.confirmed) {
        return;
      }

      this.executeSave(result.reason);
    });
  }

  private executeSave(
    reason?: string | null
  ): void {
    this.saving = true;

    const raw =
      this.form.getRawValue();

    this.variantService
      .update(
        this.variant.id,
        {
          classification:
            raw.classification ?? undefined,
          sku:
            raw.sku ?? undefined,
          minimumPercentageProfit:
            raw.minimumPercentageProfit ?? undefined,
          minimumProfit:
            raw.minimumProfit ?? undefined
        },
        reason
      )
      .subscribe({
        next: updated =>
          this.dialogRef.close(updated),
        error: () =>
          this.saving = false
      });
  }

  cancel(): void {

    this.dialogRef.close();
  }
}