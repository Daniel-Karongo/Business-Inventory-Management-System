import { Injectable } from '@angular/core';

import {
    StockOnboardingRequest
} from '../../models/stock-onboarding.model';

import {
    StockOnboardingState
} from '../models/onboarding.models';

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
                state.reference
                ?? undefined,

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

                            newSupplierName:
                                supplier.newSupplierName
                                || undefined,

                            packagingName:
                                packaging.name,

                            unitsSupplied:
                                supplier.quantity,

                            unitCost:
                                supplier.unitCost

                        };

                    })

        };

    }

}