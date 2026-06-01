import { CommonModule } from '@angular/common';
import { Component, Input, OnChanges } from '@angular/core';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatTooltipModule } from '@angular/material/tooltip';
import { Router } from '@angular/router';

import { InventoryResponse } from '../../../../models/inventory-response.model';
import { Product } from '../../../../models/product.model';
import { InventoryService } from '../../../../inventory/services/inventory.service';

@Component({
    selector: 'app-product-inventory-summary',
    standalone: true,
    imports: [
        CommonModule,
        MatButtonModule,
        MatIconModule,
        MatTooltipModule
    ],
    templateUrl: './product-inventory-summary.component.html',
    styleUrls: ['./product-inventory-summary.component.scss']
})
export class ProductInventorySummaryComponent implements OnChanges {

    @Input({ required: true })
    product!: Product;

    loading = true;

    rows: InventoryResponse[] = [];

    constructor(
        private inventoryService: InventoryService,
        private router: Router
    ) { }

    ngOnChanges(): void {
        if (!this.product?.id) return;

        this.loading = true;

        this.inventoryService.getAll().subscribe({
            next: res => {
                this.rows =
                    (res.content ?? [])
                        .filter(x => x.productId === this.product.id);

                this.loading = false;
            },
            error: () => {
                this.rows = [];
                this.loading = false;
            }
        });
    }

    available(row: InventoryResponse): number {
        return (
            row.quantityOnHand -
            row.quantityReserved
        );
    }

    totalAvailable(): number {
        return this.rows.reduce(
            (sum, r) => sum + this.available(r),
            0
        );
    }

    totalReserved(): number {
        return this.rows.reduce(
            (sum, r) => sum + r.quantityReserved,
            0
        );
    }

    totalValue(): number {
        return this.rows.reduce(
            (sum, r) => sum + (r.totalRemainingBatchValue ?? 0),
            0
        );
    }

    openTransactions(row: InventoryResponse) {
        this.router.navigate(
            ['/app/stock/inventory', row.productVariantId],
            {
                state: {
                    branchId: row.branchId
                }
            }
        );
    }

    createSale(row: InventoryResponse) {
        this.router.navigate(
            ['/app/sales/new'],
            {
                state: {
                    inventorySeed: {
                        productId: row.productId,
                        productName: row.productName,
                        variantId: row.productVariantId,
                        branchId: row.branchId
                    }
                }
            }
        );
    }
}