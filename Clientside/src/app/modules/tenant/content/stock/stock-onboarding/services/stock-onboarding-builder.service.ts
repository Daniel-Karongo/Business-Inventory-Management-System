import { Injectable } from '@angular/core';

import {
    StockOnboardingRequest
} from '../../models/stock-onboarding.model';

import {
    StockOnboardingState
} from '../models/onboarding.models';

import {
    BulkRequest
} from '../../../../../../shared/models/bulk-import.model';

import {
    OperationalExpenseInput
} from '../../models/stock-onboarding.model';

@Injectable({
    providedIn: 'root'
})
export class StockOnboardingBuilderService {

    build(
        state: StockOnboardingState
    ): StockOnboardingRequest {

        if (!state.branchId) {
            throw new Error(
                'branchId is required'
            );
        }

        return {

            productName:
                state.productDraft.name
                || undefined,

            productId:
                state.selectedProductId
                ?? undefined,

            categoryId:
                state.productDraft.categoryId ===
                    '__NEW_CATEGORY__'
                    ? undefined
                    : state.productDraft.categoryId
                    ?? undefined,

            newCategoryName:
                state.productDraft
                    .newCategoryName
                || undefined,

            minimumPercentageProfit:
                state.productDraft
                    .minimumPercentageProfit
                ?? undefined,

            classification:
                state.variantDraft
                    .classification
                || undefined,

            variantId:
                state.selectedVariantId
                ?? undefined,

            branchId:
                state.branchId,

            note:
                state.notes
                ?? undefined,

            reference:
                this.normalizeReference(
                    state.reference
                ),

            packagings:
                state.packagings
                    .map(p => ({
                        name: p.name,
                        units:
                            p.unitQuantity
                    })),

            pricing:
                state.pricing
                    .map(price => {

                        const packaging =
                            state.packagings
                                .find(
                                    p =>
                                        p.tempId ===
                                        price.packagingTempId
                                );

                        if (!packaging) {
                            throw new Error(
                                'Packaging missing during pricing mapping'
                            );
                        }

                        return {

                            packagingName:
                                packaging.name,

                            sellingPrice:
                                price.sellingPrice

                        };

                    }),

            suppliers:
                state.suppliers
                    .map(supplier => {

                        const packaging =
                            state.packagings
                                .find(
                                    p =>
                                        p.tempId ===
                                        supplier.packagingTempId
                                );

                        if (!packaging) {
                            throw new Error(
                                'Packaging missing during supplier mapping'
                            );
                        }

                        return {
                            supplierId:
                                supplier.supplierId
                                ?? undefined,
                            supplierName:
                                supplier.supplierName
                                || undefined,
                            createSupplierIfMissing:
                                !!supplier.supplierName,
                            packagingName:
                                packaging.name,
                            unitsSupplied:
                                supplier.unitsSupplied,
                            unitCost:
                                supplier.unitCost,
                            vatInclusive:
                                supplier.vatInclusive,
                            vatRate:
                                supplier.vatRate
                        };

                    }),

            accountingDate:
                state.accountingDate!,

            operationalExpenses:
                state.operationalExpenses
                    .map(
                        expense => ({
                            expenseAccountId:
                                expense.expenseAccountId,

                            description:
                                expense.description,

                            amount:
                                expense.amount
                        })
                    ),

            autoPaySuppliers:
                state.autoPaySuppliers,

            supplierPaymentMethod:
                state.supplierPaymentMethod
                ?? undefined,

            autoPayOperationalExpenses:
                state.autoPayOperationalExpenses,

            fundingAccountId:
                state.fundingAccountId
                ?? undefined

        };

    }

    private normalizeReference(
        reference?: string | null,
        type = 'RECEIPT'
    ): string {

        const value = reference?.trim();

        if (!value) {
            return `${type}:${crypto.randomUUID()}`;
        }

        // already valid TYPE:UUID
        if (
            value.includes(':') &&
            value.split(':').length === 2
        ) {
            return value;
        }

        return `${type}:${crypto.randomUUID()}`;
    }

    buildBulk(
        states: StockOnboardingState[],
        dryRun = false
    ): BulkRequest<StockOnboardingRequest> {

        return {
            items: states.map(
                state => this.build(state)
            ),
            options: {
                dryRun
            }
        };
    }

}