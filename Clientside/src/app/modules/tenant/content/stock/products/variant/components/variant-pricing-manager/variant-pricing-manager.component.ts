import { CommonModule } from '@angular/common';
import {
    Component,
    Input,
    OnInit
} from '@angular/core';

import { MatButtonModule } from '@angular/material/button';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import {
    PackagingDTO
} from '../../../../models/packaging.model';

import {
    ProductPrice
} from '../../../../models/pricing.model';

import {
    ProductVariantPricingService
} from '../../services/product-variant-pricing.service';

import {
    PricingFormComponent
} from '../pricing-form/pricing-form.component';

@Component({
    selector: 'app-variant-pricing-manager',
    standalone: true,
    imports: [
        CommonModule,
        MatButtonModule,
        MatDialogModule,
        MatSnackBarModule
    ],
    templateUrl: './variant-pricing-manager.component.html',
    styleUrls: ['./variant-pricing-manager.component.scss']
})
export class VariantPricingManagerComponent implements OnInit {

    @Input({ required: true }) variantId!: string;
    @Input({ required: true }) packaging!: PackagingDTO[];

    loading = true;

    pricing: ProductPrice[] = [];
    packagingName(packagingId?: string): string {

        if (!packagingId) {
            return 'Unknown';
        }

        return this.packaging.find(
            p => p.packagingId === packagingId
        )?.name ?? 'Unknown';
    }

    constructor(
        private service: ProductVariantPricingService,
        private dialog: MatDialog,
        private snackbar: MatSnackBar
    ) { }

    ngOnInit(): void {
        this.load();
    }

    load() {

        this.loading = true;

        this.service.getForVariant(this.variantId).subscribe({
            next: res => {
                this.pricing = res ?? [];
                this.loading = false;
            },
            error: () => {
                this.loading = false;
            }
        });
    }

    create() {

        const ref = this.dialog.open(
            PricingFormComponent,
            {
                width: '520px',
                data: {
                    packaging: this.packaging
                }
            }
        );

        ref.afterClosed().subscribe(payload => {

            if (!payload) return;

            this.service.create({
                variantId: this.variantId,
                ...payload
            }).subscribe({
                next: () => {

                    this.snackbar.open(
                        'Pricing created',
                        'Close',
                        { duration: 3000 }
                    );

                    this.load();
                }
            });
        });
    }

    edit(price: ProductPrice) {

        const ref = this.dialog.open(
            PricingFormComponent,
            {
                width: '520px',
                data: {
                    pricing: price,
                    packaging: this.packaging,
                    editMode: true
                }
            }
        );

        ref.afterClosed().subscribe(payload => {

            if (!payload) return;

            this.service.update(
                price.id!,
                payload
            ).subscribe({
                next: () => {

                    this.snackbar.open(
                        'Pricing updated',
                        'Close',
                        { duration: 3000 }
                    );

                    this.load();
                }
            });
        });
    }

    remove(price: ProductPrice) {

        this.service.remove(price.id!).subscribe({
            next: () => {

                this.pricing =
                    this.pricing.filter(
                        p => p.id! !== price.id!
                    );

                this.snackbar.open(
                    'Pricing deleted',
                    'Close',
                    { duration: 3000 }
                );
            }
        });
    }
}