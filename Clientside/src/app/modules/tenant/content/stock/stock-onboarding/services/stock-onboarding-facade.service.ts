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
    OnboardingOperationalExpense,
    OnboardingPackagingDraft,
    OnboardingPricingDraft,
    OnboardingSupplierEntry,
    OnboardingVariantDraft,
    StockOnboardingState,
    SupplierPaymentMethod
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

import {
    BulkResult
} from '../../../../../../shared/models/bulk-import.model';

import {
    StockOnboardingBulkPreviewResult
} from '../../models/stock-onboarding.model';

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

    readonly bulkSubmitting =
        signal(false);

    readonly bulkPreviewResult =
        signal<
            BulkResult<
                StockOnboardingBulkPreviewResult
            > | null
        >(null);

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
                        sum + row.unitsSupplied,
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
                            row.unitsSupplied *
                            row.unitCost
                        ),
                    0
                )
        );

    setMode(mode: OnboardingMode) {

        if (mode === 'NEW_PRODUCT') {

            this.patch({

                mode,

                selectedProductId: null

            });

            return;
        }

        this.patch({ mode });

    }

    setCurrentStep(currentStep: number) {
        this.patch({ currentStep });
    }

    setBranch(branchId: string | null) {
        this.patch({ branchId });
    }

    setSelectedProduct(
        product: {
            id: string;
            name?: string;
            description?: string;
        } | null
    ) {

        this.patch({

            selectedProductId:
                product?.id ?? null,

            productDraft: {

                ...this.state().productDraft,

                name:
                    product?.name ?? '',

                description:
                    product?.description ?? ''

            }

        });

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
                    supplierName:
                        row.supplierName?.trim()
                        || null,
                    packagingTempId:
                        row.packagingTempId,
                    unitsSupplied:
                        Number(row.unitsSupplied),
                    unitCost:
                        Number(row.unitCost),
                    vatInclusive:
                        !!row.vatInclusive,
                    vatRate:
                        Number(row.vatRate ?? 0)
                }))
        });
    }

    setAccountingDate(
        accountingDate: string | null
    ) {
        this.patch({
            accountingDate
        });
    }

    setOperationalExpenses(
        operationalExpenses:
            OnboardingOperationalExpense[]
    ) {
        this.patch({
            operationalExpenses
        });
    }

    setAutoPaySuppliers(
        autoPaySuppliers: boolean
    ) {
        this.patch({
            autoPaySuppliers
        });
    }

    setSupplierPaymentMethod(
        supplierPaymentMethod:
            SupplierPaymentMethod | null
    ) {
        this.patch({
            supplierPaymentMethod
        });
    }

    setAutoPayOperationalExpenses(
        autoPayOperationalExpenses:
            boolean
    ) {
        this.patch({
            autoPayOperationalExpenses
        });
    }

    setFundingAccount(
        fundingAccountId:
            string | null
    ) {
        this.patch({
            fundingAccountId
        });
    }

    updateReviewState(
        reviewState:
            Partial<StockOnboardingState>
    ) {

        const patch:
            Partial<StockOnboardingState> = {};

        if (
            reviewState.accountingDate !== undefined
        ) {
            patch.accountingDate =
                reviewState.accountingDate;
        }

        if (
            reviewState.operationalExpenses !== undefined
        ) {
            patch.operationalExpenses =
                reviewState.operationalExpenses;
        }

        if (
            reviewState.autoPaySuppliers !== undefined
        ) {
            patch.autoPaySuppliers =
                reviewState.autoPaySuppliers;
        }

        if (
            reviewState.supplierPaymentMethod !== undefined
        ) {
            patch.supplierPaymentMethod =
                reviewState.supplierPaymentMethod;
        }

        if (
            reviewState.autoPayOperationalExpenses !== undefined
        ) {
            patch.autoPayOperationalExpenses =
                reviewState.autoPayOperationalExpenses;
        }

        if (
            reviewState.fundingAccountId !== undefined
        ) {
            patch.fundingAccountId =
                reviewState.fundingAccountId;
        }

        this.patch(patch);

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
            mode,
            branchId,
            selectedProductId,
            productDraft,
            variantDraft,
            packagings,
            pricing,
            suppliers
        } = this.state();

        switch (step) {

            case 0: {

                const productErrors: string[] = [];

                if (
                    mode === 'EXISTING_PRODUCT'
                ) {

                    if (
                        !selectedProductId
                    ) {

                        productErrors.push(
                            'Please select a product.'
                        );

                    }

                    return productErrors;
                }

                if (
                    !productDraft.name?.trim()
                ) {

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
            }

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

            case 5:
                return this.validator
                    .validateAccountingAndPayments(
                        this.state()
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

            accountingDate:
                new Date()
                    .toISOString()
                    .substring(0, 10),

            operationalExpenses: [],

            autoPaySuppliers: false,

            supplierPaymentMethod: null,

            autoPayOperationalExpenses: false,

            fundingAccountId: null,

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
                        '/app/stock'
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

    buildCurrentState() {

        return this.builder.build(
            this.state()
        );
    }

    previewBulk(
        states: any[]
    ) {

        if (this.bulkSubmitting()) {
            return;
        }

        this.bulkSubmitting.set(true);

        const payload =
            this.builder.buildBulk(
                states,
                true
            );

        this.onboardingService
            .previewBulk(payload)
            .pipe(
                finalize(() =>
                    this.bulkSubmitting.set(false)
                )
            )
            .subscribe({
                next: result => {

                    this.bulkPreviewResult.set(
                        result
                    );

                    this.snackbar.open(
                        'Bulk preview generated',
                        'Close',
                        { duration: 4000 }
                    );
                },

                error: err => {

                    this.snackbar.open(
                        err?.error?.message
                        || 'Bulk preview failed',
                        'Close',
                        { duration: 5000 }
                    );

                    console.error(
                        'Bulk preview failed',
                        err
                    );
                }
            });
    }

    submitBulk(
        states: any[]
    ) {

        if (this.bulkSubmitting()) {
            return;
        }

        this.bulkSubmitting.set(true);

        const payload =
            this.builder.buildBulk(
                states,
                false
            );

        this.onboardingService
            .bulkCreate(payload)
            .pipe(
                finalize(() =>
                    this.bulkSubmitting.set(false)
                )
            )
            .subscribe({

                next: result => {

                    this.bulkPreviewResult.set(
                        result
                    );

                    const failed =
                        result.failed || 0;

                    const success =
                        result.success || 0;

                    if (failed > 0) {

                        this.snackbar.open(
                            `${success} rows processed, ${failed} failed`,
                            'Close',
                            { duration: 6000 }
                        );

                    } else {

                        this.snackbar.open(
                            'Bulk onboarding completed',
                            'Close',
                            { duration: 4000 }
                        );
                    }

                    this.router.navigate([
                        '/app/inventory'
                    ]);
                },

                error: err => {

                    this.snackbar.open(
                        err?.error?.message
                        || 'Bulk onboarding failed',
                        'Close',
                        { duration: 5000 }
                    );

                    console.error(
                        'Bulk onboarding failed',
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