import { CommonModule } from '@angular/common';
import { Component, Input, OnChanges } from '@angular/core';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatTooltipModule } from '@angular/material/tooltip';
import { Router } from '@angular/router';

import { InventoryService } from '../../../../inventory/services/inventory.service';
import { InventoryResponse, InventoryWorkspaceResponse } from '../../../../models/inventory-response.model';
import { Product } from '../../../../models/product.model';

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

    rows: InventoryWorkspaceResponse[] = [];

    constructor(
        private inventoryService: InventoryService,
        private router: Router
    ) { }

    ngOnChanges(): void {
        if (!this.product?.id || !this.product?.branchId) {
            this.rows = [];
            return;
        }

        this.loading = true;

        this.inventoryService
            .getProductInBranch(
                this.product.id,
                this.product.branchId
            )
            .subscribe({
                next: rows => {
                    this.rows = rows ?? [];
                    this.loading = false;
                },
                error: () => {
                    this.rows = [];
                    this.loading = false;
                }
            });
    }

    averageMargin(): number {

        return this.rows.reduce(
            (sum, r) =>
                sum +
                (r.projectedMarginPercent ?? 0),
            0
        ) / Math.max(this.rows.length, 1);

    }

    available(row: InventoryWorkspaceResponse): number {
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

    openTransactions(row: InventoryWorkspaceResponse) {
        this.router.navigate(
            ['/app/stock/inventory', row.productVariantId],
            {
                state: {
                    branchId: row.branchId
                }
            }
        );
    }

    createSale(row: InventoryWorkspaceResponse) {
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