import {
    CommonModule
} from '@angular/common';

import {
    Component,
    Inject,
    OnInit
} from '@angular/core';

import {
    FormsModule
} from '@angular/forms';

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
    MatTabsModule
} from '@angular/material/tabs';

import {
    MatProgressSpinnerModule
} from '@angular/material/progress-spinner';

import {
    VariantCommercialSummaryComponent
} from '../variant-commercial-summary/variant-commercial-summary.component';

import {
    ProductVariant,
    VariantAudit
} from '../../../../models/product-variant.model';

import {
    ProductVariantService
} from '../../services/product-variant.service';

import {
    VariantAuditsComponent
} from '../variant-audits/variant-audits.component';

@Component({
    selector:
        'app-variant-details-dialog',

    standalone: true,

    imports: [
        CommonModule,
        FormsModule,
        MatDialogModule,
        MatButtonModule,
        MatIconModule,
        MatTabsModule,
        MatProgressSpinnerModule,
        VariantCommercialSummaryComponent,
        VariantAuditsComponent
    ],

    templateUrl:
        './variant-details-dialog.component.html',

    styleUrls: [
        './variant-details-dialog.component.scss'
    ]
})
export class VariantDetailsDialogComponent
    implements OnInit {

    loading = true;

    variant!: ProductVariant;

    auditCount = 0;

    constructor(
        @Inject(MAT_DIALOG_DATA)
        public data: {
            variant: ProductVariant;
            branchId?: string;
        },

        private variantService:
            ProductVariantService
    ) { }

    ngOnInit(): void {

        this.variant =
            this.data.variant;

        this.loading = false;

        this.loadAuditCount();
    }

    private loadAuditCount(): void {

        this.variantService
            .getAuditHistory(
                this.data.variant.id,
                this.data.branchId
            )
            .subscribe({
                next: audits => {

                    this.auditCount =
                        audits?.length ?? 0;
                }
            });
    }

    get statusLabel():
        string {

        return this.variant.deleted
            ? 'Deleted'
            : 'Active';
    }
}