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
        minimumPercentageProfit: null
    },

    variantDraft: {
        classification: '',
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