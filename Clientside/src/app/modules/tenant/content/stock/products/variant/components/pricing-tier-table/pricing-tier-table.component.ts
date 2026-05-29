import { CommonModule } from '@angular/common';
import {
    Component,
    Input
} from '@angular/core';

import {
    PackagingDTO
} from '../../../../models/packaging.model';

import {
    ProductPrice
} from '../../../../models/pricing.model';

@Component({
    selector: 'app-pricing-tier-table',
    standalone: true,
    imports: [CommonModule],
    templateUrl: './pricing-tier-table.component.html',
    styleUrls: ['./pricing-tier-table.component.scss']
})
export class PricingTierTableComponent {

    @Input({ required: true }) pricing: ProductPrice[] = [];
    @Input({ required: true }) packaging: PackagingDTO[] = [];

    rows(packagingId: string): ProductPrice[] {
        return this.pricing
            .filter(
                p => p.packagingId === packagingId
            )
            .sort(
                (a, b) =>
                    (a.minQuantity ?? 1) -
                    (b.minQuantity ?? 1)
            );
    }
}