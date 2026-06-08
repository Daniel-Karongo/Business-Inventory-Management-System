import { CommonModule } from '@angular/common';
import {
    Component,
    EventEmitter,
    Input,
    OnInit,
    Output
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
import { MatIconModule } from '@angular/material/icon';
import { MatTooltipModule } from '@angular/material/tooltip';

@Component({
    selector: 'app-variant-pricing-manager',
    standalone: true,
    imports: [
        CommonModule,
        MatButtonModule,
        MatDialogModule,
        MatSnackBarModule,
        MatIconModule,
        MatTooltipModule
    ],
    templateUrl: './variant-pricing-manager.component.html',
    styleUrls: ['./variant-pricing-manager.component.scss']
})
export class VariantPricingManagerComponent {

    @Input({ required: true }) variantId!: string;
    @Input({ required: true }) packaging!: PackagingDTO[];
    @Output()
    refreshRequested =
        new EventEmitter<void>();

    @Input({ required: true })
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

    create() {

        const ref = this.dialog.open(
            PricingFormComponent,
            {
                width: 'min(640px, 96vw)',
                maxWidth: '96vw',
                panelClass: 'enterprise-dialog',
                autoFocus: false,
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

                    this.refreshRequested.emit();
                },

                error: err => {
                    this.snackbar.open(
                        err?.error?.message
                        ?? 'Failed to create pricing',
                        'Close',
                        { duration: 7000 }
                    );
                }
            });
        });
    }

    edit(price: ProductPrice) {

        const ref = this.dialog.open(
            PricingFormComponent,
            {
                width: 'min(640px, 96vw)',
                maxWidth: '96vw',
                panelClass: 'enterprise-dialog',
                autoFocus: false,
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

                    this.refreshRequested.emit();
                },

                error: err => {
                    this.snackbar.open(
                        err?.error?.message
                        ?? 'Failed to create pricing',
                        'Close',
                        { duration: 7000 }
                    );
                }
            });
        });
    }

    remove(price: ProductPrice) {

        this.service.remove(price.id!).subscribe({
            next: () => {

                this.snackbar.open(
                    'Pricing deleted',
                    'Close',
                    { duration: 3000 }
                );

                this.refreshRequested.emit();
            },

            error: err => {
                this.snackbar.open(
                    err?.error?.message
                    ?? 'Failed to create pricing',
                    'Close',
                    { duration: 7000 }
                );
            }
        });
    }
}