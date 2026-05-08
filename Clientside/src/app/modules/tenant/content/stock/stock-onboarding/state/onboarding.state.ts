import { Injectable, signal } from '@angular/core';

import {
    StockOnboardingState
} from '../models/onboarding.models';

export const initialOnboardingState:
    StockOnboardingState = {

    currentStep: 0,

    mode: 'NEW_PRODUCT',

    branchId: null,

    selectedProductId: null,

    selectedVariantId: null,

    productDraft: {
        name: '',
        description: '',
        categoryId: null,
        newCategoryName: null,
        minimumPercentageProfit: null
    },

    variantDraft: {
        classification: 'STANDARD',
        sku: null,
        barcode: null
    },

    packagings: [],

    pricing: [],

    suppliers: [],

    notes: null,

    reference: null
};

@Injectable()
export class OnboardingState {

    readonly state =
        signal<StockOnboardingState>(
            initialOnboardingState
        );

}