import {
    Component,
    Inject
} from '@angular/core';

import {
    CommonModule
} from '@angular/common';

import {
    MAT_DIALOG_DATA,
    MatDialogModule,
    MatDialogRef
} from '@angular/material/dialog';

import {
    FormControl,
    ReactiveFormsModule
} from '@angular/forms';

import {
    MatFormFieldModule
} from '@angular/material/form-field';

import {
    MatInputModule
} from '@angular/material/input';

import {
    MatIconModule
} from '@angular/material/icon';

import {
    MatButtonModule
} from '@angular/material/button';
import { ProductVariant } from '../../../../models/product-variant.model';

@Component({
    selector: 'app-variant-selector-dialog',
    standalone: true,
    imports: [
        CommonModule,
        MatDialogModule,
        ReactiveFormsModule,
        MatFormFieldModule,
        MatInputModule,
        MatIconModule,
        MatButtonModule
    ],
    templateUrl:
        './variant-selector-dialog.component.html',
    styleUrls: [
        './variant-selector-dialog.component.scss'
    ]
})
export class VariantSelectorDialogComponent {

    search =
        new FormControl(
            '',
            {
                nonNullable: true
            }
        );

    constructor(
        @Inject(MAT_DIALOG_DATA)
        public data: {
            productName: string;
            variants: ProductVariant[];
        },
        private dialogRef:
            MatDialogRef<
                VariantSelectorDialogComponent,
                ProductVariant
            >
    ) { }

    get filteredVariants():
        ProductVariant[] {

        const q =
            this.search.value
                .trim()
                .toLowerCase();

        if (!q) {
            return this.data.variants;
        }

        return this.data.variants.filter(
            variant =>
                variant.classification
                    ?.toLowerCase()
                    .includes(q)
                ||
                variant.sku
                    ?.toLowerCase()
                    .includes(q)
        );
    }

    select(
        variant: ProductVariant
    ): void {

        this.dialogRef.close(
            variant
        );

    }

    cancel(): void {

        this.dialogRef.close();

    }
}