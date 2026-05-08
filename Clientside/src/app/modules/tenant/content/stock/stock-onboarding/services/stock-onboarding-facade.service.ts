import {
    Injectable,
    computed,
    inject,
    signal
} from '@angular/core';

import { Router } from '@angular/router';

import { finalize } from 'rxjs';

import { MatSnackBar } from '@angular/material/snack-bar';

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
    StockOnboardingValidatorService
} from './stock-onboarding-validator.service';

@Injectable()
export class StockOnboardingFacadeService {

    private readonly router = inject(Router);

    private readonly snackbar =
        inject(MatSnackBar);

    private readonly onboardingState =
        inject(OnboardingState);

    private readonly onboardingService =
        inject(StockOnboardingService);

    private readonly builder =
        inject(StockOnboardingBuilderService);

    private readonly validator =
        inject(StockOnboardingValidatorService);

    readonly submitting =
        signal(false);

    readonly state =
        this.onboardingState.state;

    readonly currentStep =
        computed(() =>
            this.state().currentStep
        );

    readonly totalUnits =
        computed(() =>
            this.state()
                .suppliers
                .reduce(
                    (sum, row) =>
                        sum + row.quantity,
                    0
                )
        );

    readonly totalStockValue =
        computed(() =>
            this.state()
                .suppliers
                .reduce(
                    (sum, row) =>
                        sum + (
                            row.quantity *
                            row.unitCost
                        ),
                    0
                )
        );

    setMode(mode: OnboardingMode) {
        this.patch({ mode });
    }

    setCurrentStep(currentStep: number) {
        this.patch({ currentStep });
    }

    setBranch(branchId: string | null) {
        this.patch({ branchId });
    }

    setSelectedProduct(
        selectedProductId: string | null
    ) {
        this.patch({ selectedProductId });
    }

    setVariantDraft(
        variantDraft: OnboardingVariantDraft
    ) {

        this.patch({
            variantDraft: {
                classification:
                    variantDraft.classification?.trim()
                    || 'STANDARD',

                sku:
                    variantDraft.sku ?? null,

                barcode:
                    variantDraft.barcode ?? null
            }
        });

    }

    setProductDraft(draft: any) {

        this.patch({

            branchId:
                draft.branchId ?? null,

            productDraft: {

                name:
                    draft.name ?? '',

                description:
                    draft.description ?? '',

                categoryId:
                    draft.categoryId ?? null,

                newCategoryName:
                    draft.newCategoryName?.trim() ?? null,

                minimumPercentageProfit:
                    draft.minimumPercentageProfit
                    ?? null
            }

        });

    }

    setPackagings(
        packagings:
            OnboardingPackagingDraft[]
    ) {

        this.patch({ packagings });

    }

    setPricing(
        pricing:
            OnboardingPricingDraft[]
    ) {

        this.patch({

            pricing:
                pricing.map(row => ({

                    packagingTempId:
                        row.packagingTempId,

                    sellingPrice:
                        Number(row.sellingPrice)

                }))

        });

    }

    setSuppliers(
        suppliers:
            OnboardingSupplierEntry[]
    ) {

        this.patch({

            suppliers:
                suppliers.map(row => ({

                    supplierId:
                        row.supplierId ?? null,

                    newSupplierName:
                        row.newSupplierName?.trim()
                        || null,

                    packagingTempId:
                        row.packagingTempId,

                    quantity:
                        Number(row.quantity),

                    unitCost:
                        Number(row.unitCost)

                }))

        });

    }

    nextStep() {

        const currentStep =
            this.state().currentStep;

        const errors =
            this.validateStep(
                currentStep
            );

        if (errors.length) {

            this.snackbar.open(
                errors[0],
                'Close',
                { duration: 4000 }
            );

            return;

        }

        this.patch({

            currentStep:
                currentStep + 1

        });

    }

    previousStep() {

        this.patch({

            currentStep:
                Math.max(
                    0,
                    this.state()
                        .currentStep - 1
                )

        });

    }

    navigateToStep(targetStep: number) {

        const maxStep = 5;

        if (
            targetStep < 0 ||
            targetStep > maxStep
        ) {
            return;
        }

        const currentStep =
            this.state().currentStep;

        // same step
        if (targetStep === currentStep) {
            return;
        }

        // backwards always allowed
        if (targetStep < currentStep) {

            this.patch({
                currentStep: targetStep
            });

            return;
        }

        // validate all intermediate steps
        for (
            let step = currentStep;
            step < targetStep;
            step++
        ) {

            const errors =
                this.validateStep(step);

            if (errors.length) {

                this.snackbar.open(
                    errors[0],
                    'Close',
                    { duration: 4000 }
                );

                return;
            }

        }

        this.patch({
            currentStep: targetStep
        });

    }

    private validateStep(
        step: number
    ): string[] {

        const {
            branchId,
            productDraft,
            variantDraft,
            packagings,
            pricing,
            suppliers
        } = this.state();

        switch (step) {

            case 0:

                const productErrors: string[] = [];

                if (!productDraft.name?.trim()) {

                    productErrors.push(
                        'Product name is required.'
                    );

                }

                if (
                    !productDraft.categoryId &&
                    !productDraft.newCategoryName?.trim()
                ) {

                    productErrors.push(
                        'Category is required.'
                    );

                }

                if (!branchId) {

                    productErrors.push(
                        'Branch is required.'
                    );

                }

                return productErrors;

            case 1:

                return !variantDraft
                    .classification
                    ?.trim()
                    ? [
                        'Variant classification is required.'
                    ]
                    : [];

            case 2:

                return this.validator
                    .validatePackagings(
                        packagings
                    );

            case 3:

                return this.validator
                    .validatePricing(
                        pricing
                    );

            case 4:

                return this.validator
                    .validateSuppliers(
                        suppliers
                    );

            default:

                return [];

        }

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

        });

    }

    submit() {

        if (this.submitting()) {
            return;
        }

        this.submitting.set(true);

        this.onboardingService
            .create(
                this.builder.build(
                    this.state()
                )
            )
            .pipe(
                finalize(() =>
                    this.submitting.set(false)
                )
            )
            .subscribe({

                next: response => {

                    this.reset();

                    this.snackbar.open(

                        response.message ||
                        'Stock onboarded successfully',

                        'Close',

                        { duration: 4000 }

                    );

                    this.router.navigate([
                        '/app/inventory'
                    ]);

                },

                error: err => {

                    this.snackbar.open(

                        err?.error?.message ||
                        'Onboarding failed',

                        'Close',

                        { duration: 5000 }

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