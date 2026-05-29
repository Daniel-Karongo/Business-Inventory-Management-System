import { CommonModule } from '@angular/common';
import {
    Component,
    Inject
} from '@angular/core';

import {
    FormBuilder,
    ReactiveFormsModule,
    Validators
} from '@angular/forms';

import { MAT_DIALOG_DATA, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';

import {
    PackagingDTO
} from '../../../../models/packaging.model';

import {
    ProductPrice
} from '../../../../models/pricing.model';

@Component({
    selector: 'app-pricing-form',
    standalone: true,
    imports: [
        CommonModule,
        ReactiveFormsModule,
        MatDialogModule,
        MatButtonModule,
        MatInputModule,
        MatSelectModule
    ],
    templateUrl: './pricing-form.component.html',
    styleUrls: ['./pricing-form.component.scss']
})
export class PricingFormComponent {

    saving = false;

    form!: ReturnType<FormBuilder['group']>;

    constructor(
        private fb: FormBuilder,
        private ref: MatDialogRef<PricingFormComponent>,
        @Inject(MAT_DIALOG_DATA)
        public data: {
            pricing?: ProductPrice;
            packaging: PackagingDTO[];
            editMode?: boolean;
        }
    ) {
        this.form = this.fb.group({
            packagingId: ['', Validators.required],
            price: [0, [Validators.required, Validators.min(0)]],
            minQuantity: [1, [Validators.required, Validators.min(1)]]
        });
        
        if (data?.pricing) {

            this.form.patchValue({
                packagingId: data.pricing.packagingId ?? '',
                price: data.pricing.price,
                minQuantity: data.pricing.minQuantity
            });

            this.form.get('packagingId')?.disable();
            this.form.get('minQuantity')?.disable();
        }
    }

    submit() {

        if (this.form.invalid || this.saving) {
            this.form.markAllAsTouched();
            return;
        }

        this.saving = true;

        this.ref.close(this.form.getRawValue());
    }

    close() {
        this.ref.close();
    }
}