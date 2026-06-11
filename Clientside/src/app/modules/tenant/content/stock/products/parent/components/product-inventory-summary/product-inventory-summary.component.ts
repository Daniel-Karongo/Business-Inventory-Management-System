import { CommonModule } from '@angular/common';
import {
    Component,
    Input,
    OnChanges
} from '@angular/core';

import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatTooltipModule } from '@angular/material/tooltip';

import { Router } from '@angular/router';

import { MatDialog } from '@angular/material/dialog';

import { InventoryService }
    from '../../../../inventory/services/inventory.service';

import {
    InventoryResponse,
    InventoryWorkspaceResponse
} from '../../../../models/inventory-response.model';

import { Product }
    from '../../../../models/product.model';

import { ReceiveStockDialogComponent }
    from '../../../../inventory/components/receive-stock-dialog/receive-stock-dialog.component';

import { TransferStockDialogComponent }
    from '../../../../inventory/components/transfer-stock-dialog/transfer-stock-dialog.component';

import { AdjustStockDialogComponent }
    from '../../../../inventory/components/adjust-stock-dialog/adjust-stock-dialog.component';
import { BranchContextService } from '../../../../../../../../core/services/branch-context.service';

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
        private router: Router,
        private dialog: MatDialog,
        private branchContext: BranchContextService
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

    get canTransferStock(): boolean {
        return this.branchContext.branches.length > 1;
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

    receiveStock(
        row: InventoryWorkspaceResponse
    ): void {

        const ref =
            this.dialog.open(
                ReceiveStockDialogComponent,
                {
                    width: '720px',
                    maxWidth: '95vw',
                    data: {
                        productId:
                            row.productId,

                        productName:
                            row.productName,

                        productVariantId:
                            row.productVariantId,

                        classification:
                            row.productClassification,

                        branchId:
                            row.branchId,

                        branchName:
                            row.branchName
                    }
                }
            );

        ref.afterClosed()
            .subscribe(success => {

                if (success) {

                    this.ngOnChanges();

                }

            });

    }

    transferStock(
        row: InventoryWorkspaceResponse
    ): void {

        const ref =
            this.dialog.open(
                TransferStockDialogComponent,
                {
                    width: '720px',
                    maxWidth: '95vw',

                    data: {

                        productVariantId:
                            row.productVariantId,

                        productName:
                            row.productName,

                        classification:
                            row.productClassification,

                        fromBranchId:
                            row.branchId,

                        fromBranchName:
                            row.branchName,

                        available:
                            row.quantityAvailable,

                        averageCost:
                            row.averageCost
                    }
                }
            );

        ref.afterClosed()
            .subscribe(success => {

                if (success) {

                    this.ngOnChanges();

                }

            });

    }

    adjustStock(
        row: InventoryWorkspaceResponse
    ): void {

        const ref =
            this.dialog.open(
                AdjustStockDialogComponent,
                {
                    width: '560px',
                    maxWidth: '95vw',

                    data: {

                        productVariantId:
                            row.productVariantId,

                        productName:
                            row.productName,

                        classification:
                            row.productClassification,

                        branchId:
                            row.branchId,

                        branchName:
                            row.branchName
                    }
                }
            );

        ref.afterClosed()
            .subscribe(success => {

                if (success) {

                    this.ngOnChanges();

                }

            });

    }
}