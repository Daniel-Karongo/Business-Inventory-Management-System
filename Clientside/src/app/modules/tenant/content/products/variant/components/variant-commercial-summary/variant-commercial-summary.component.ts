import { CommonModule } from '@angular/common';
import {
    Component,
    Input,
    OnChanges,
    SimpleChanges
} from '@angular/core';

import { MatCardModule } from '@angular/material/card';

import {
    ProductVariant
} from '../../../../stock/models/product-variant.model';

import {
    PackagingDTO
} from '../../../../stock/models/packaging.model';

import {
    ProductPrice
} from '../../../../stock/models/pricing.model';

import {
    ProductVariantPackagingService
} from '../../services/product-variant-packaging.service';

import {
    ProductVariantPricingService
} from '../../services/product-variant-pricing.service';

import {
    VariantPackagingManagerComponent
} from '../variant-packaging-manager/variant-packaging-manager.component';

import {
    VariantPricingManagerComponent
} from '../variant-pricing-manager/variant-pricing-manager.component';
import { PricingTierTableComponent } from '../pricing-tier-table/pricing-tier-table.component';
import { PackagingConversionTableComponent } from '../packaging-conversion-table/packaging-conversion-table.component';
import { MatExpansionModule } from '@angular/material/expansion';
import { MatTabsModule } from '@angular/material/tabs';

@Component({
    selector: 'app-variant-commercial-summary',
    standalone: true,
    imports: [
        CommonModule,
        MatCardModule,
        MatExpansionModule,
        MatTabsModule,
        VariantPackagingManagerComponent,
        VariantPricingManagerComponent,
        PricingTierTableComponent,
        PackagingConversionTableComponent,
    ],
    templateUrl: './variant-commercial-summary.component.html',
    styleUrls: ['./variant-commercial-summary.component.scss']
})
export class VariantCommercialSummaryComponent implements OnChanges {

    @Input({ required: true }) variant!: ProductVariant;
    @Input() branchId?: string;

    loading = false;
    loaded = false;
    expanded = false;

    packagings: PackagingDTO[] = [];
    pricing: ProductPrice[] = [];

    constructor(
        private packagingService: ProductVariantPackagingService,
        private pricingService: ProductVariantPricingService
    ) { }

    ngOnChanges(changes: SimpleChanges): void {

        if (
            changes['variant'] &&
            !changes['variant'].firstChange
        ) {
            this.loaded = false;
            this.expanded = false;
        }
    }

    load() {

        this.loading = true;

        this.packagingService
            .getForVariant(this.variant.id)
            .subscribe({
                next: packagings => {

                    this.packagings = packagings ?? [];

                    this.pricingService
                        .getForVariant(this.variant.id)
                        .subscribe({
                            next: pricing => {
                                this.pricing = pricing ?? [];
                                this.loading = false;
                            },
                            error: () => {
                                this.loading = false;
                            }
                        });

                },
                error: () => {
                    this.loading = false;
                }
            });
    }

    toggle(expanded: boolean) {

        this.expanded = expanded;

        if (
            expanded &&
            !this.loaded
        ) {

            this.loaded = true;

            this.load();
        }
    }

    packagingPricingCount(packagingId: string): number {

        return this.pricing.filter(
            p => p.packaging?.id === packagingId
        ).length;
    }
}