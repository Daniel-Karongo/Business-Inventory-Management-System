import {
    CommonModule
} from '@angular/common';

import {
    Component,
    Inject
} from '@angular/core';

import {
    MAT_DIALOG_DATA,
    MatDialogModule
} from '@angular/material/dialog';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatIconModule
} from '@angular/material/icon';

import {
    VariantCommercialSummaryComponent
} from '../variant-commercial-summary/variant-commercial-summary.component';
import { ProductVariant } from '../../../../models/product-variant.model';

@Component({
    selector:
        'app-variant-details-dialog',

    standalone: true,

    imports: [
        CommonModule,
        MatDialogModule,
        MatButtonModule,
        MatIconModule,
        VariantCommercialSummaryComponent
    ],

    templateUrl:
        './variant-details-dialog.component.html',

    styleUrls: [
        './variant-details-dialog.component.scss'
    ]
})
export class VariantDetailsDialogComponent {

    constructor(
        @Inject(MAT_DIALOG_DATA)
        public data: {
            variant: ProductVariant;
            branchId?: string;
        }
    ) { }
}