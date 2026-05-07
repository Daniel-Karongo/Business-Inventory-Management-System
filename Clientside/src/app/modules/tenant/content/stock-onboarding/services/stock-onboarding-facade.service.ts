import {
    Injectable,
    computed,
    inject
} from '@angular/core';

import { Router } from '@angular/router';

import {
    finalize
} from 'rxjs';

import { OnboardingState } from '../state/onboarding.state';

import {
    OnboardingMode,
    OnboardingPackagingDraft,
    OnboardingPricingDraft,
    OnboardingSupplierEntry,
    OnboardingVariantDraft
} from '../models/onboarding.models';

import {
    StockOnboardingBuilderService
} from './stock-onboarding-builder.service';

import {
    StockOnboardingService
} from './stock-onboarding.service';
import {
    signal
} from '@angular/core';

import {
    MatSnackBar
} from '@angular/material/snack-bar';

import {
    StockOnboardingValidatorService
} from './stock-onboarding-validator.service';

@Injectable()
export class StockOnboardingFacadeService {

    private readonly builder =
        inject(
            StockOnboardingBuilderService
        );

    private readonly onboardingService =
        inject(
            StockOnboardingService
        );

    private readonly router =
        inject(Router);

    private readonly onboardingState =
        inject(OnboardingState);

    private readonly snackbar =
        inject(MatSnackBar);

    private readonly validator =
        inject(
            StockOnboardingValidatorService
        );

    readonly submitting =
        signal(false);

    readonly state =
        this.onboardingState.state;

    readonly currentStep =
        computed(() =>
            this.state().currentStep
        );

    readonly totalStockValue =
        computed(() => {

            return this.state()
                .suppliers
                .reduce(
                    (sum, row) =>
                        sum + (
                            row.quantity *
                            row.unitCost
                        ),
                    0
                );

        });

    readonly totalUnits =
        computed(() => {

            return this.state()
                .suppliers
                .reduce(
                    (sum, row) =>
                        sum + row.quantity,
                    0
                );

        });

    setMode(mode: OnboardingMode) {

        this.patch({
            mode
        });

    }

    setCurrentStep(step: number) {

        this.patch({
            currentStep: step
        });

    }

    setBranch(branchId: string | null) {

        this.patch({
            branchId
        });

    }

    setSelectedProduct(
        productId: string | null
    ) {

        this.patch({
            selectedProductId: productId
        });

    }

    setVariant(
        variant: OnboardingVariantDraft
    ) {

        this.patch({
            variantDraft: variant
        });

    }

    setProductDraft(
        draft: any
    ) {

        this.patch({
            productDraft: draft
        });

    }

    setVariantDraft(
        variant:
            OnboardingVariantDraft
    ) {

        this.patch({
            variantDraft: variant
        });

    }

    setPackagings(
        packagings:
            OnboardingPackagingDraft[]
    ) {

        this.patch({
            packagings
        });

    }

    setPricing(
        pricing:
            OnboardingPricingDraft[]
    ) {

        this.patch({
            pricing
        });

    }

    setSuppliers(
        suppliers:
            OnboardingSupplierEntry[]
    ) {

        this.patch({
            suppliers
        });

    }

    nextStep() {

        const step =
            this.state().currentStep;

        let errors: string[] = [];

        switch (step) {

            case 0:

                if (
                    !this.state()
                        .productDraft
                        .name
                        ?.trim()
                ) {

                    errors.push(
                        'Product name is required.'
                    );

                }

                if (
                    !this.state()
                        .branchId
                ) {

                    errors.push(
                        'Branch is required.'
                    );

                }

                break;

            case 1:

                if (
                    !this.state()
                        .variantDraft
                        .classification
                        ?.trim()
                ) {

                    errors.push(
                        'Variant classification is required.'
                    );

                }

                break;

            case 2:

                errors =
                    this.validator
                        .validatePackagings(
                            this.state()
                                .packagings
                        );

                break;

            case 3:

                errors =
                    this.validator
                        .validatePricing(
                            this.state()
                                .pricing
                        );

                break;

            case 4:

                errors =
                    this.validator
                        .validateSuppliers(
                            this.state()
                                .suppliers
                        );

                break;

        }

        if (errors.length) {

            this.snackbar.open(
                errors[0],
                'Close',
                {
                    duration: 4000
                }
            );

            return;

        }

        this.patch({
            currentStep:
                this.state()
                    .currentStep + 1
        });

    }

    previousStep() {

        this.patch({
            currentStep:
                Math.max(
                    0,
                    this.state().currentStep - 1
                )
        });

    }

    reset() {

        this.onboardingState.state.set({
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
        });

    }

    submit() {

        if (
            this.submitting()
        ) {
            return;
        }

        this.submitting.set(
            true
        );

        const payload =
            this.builder.build(
                this.state()
            );

        this.onboardingService
            .create(payload)
            .pipe(
                finalize(() => {

                    this.submitting.set(
                        false
                    );

                })
            )
            .subscribe({

                next: response => {

                    this.reset();

                    this.snackbar.open(
                        response.message ||
                        'Stock onboarded successfully',
                        'Close',
                        {
                            duration: 4000
                        }
                    );

                    this.router.navigate([
                        '/app/inventory'
                    ]);

                },

                error: err => {

                    this.snackbar.open(
                        err?.error?.message
                        || 'Onboarding failed',
                        'Close',
                        {
                            duration: 5000
                        }
                    );

                    console.error(
                        'Onboarding failed',
                        err
                    );

                }

            });

    }

    private patch(
        partial: Partial<any>
    ) {

        this.onboardingState.state.update(
            current => ({
                ...current,
                ...partial
            })
        );

    }

}