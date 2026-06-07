import { Injectable } from '@angular/core';

import {
    OnboardingPackagingDraft,
    OnboardingPricingDraft,
    OnboardingSupplierEntry,
    StockOnboardingState
} from '../models/onboarding.models';

@Injectable({
    providedIn: 'root'
})
export class StockOnboardingValidatorService {

    validatePackagings(
        packagings:
            OnboardingPackagingDraft[]
    ): string[] {

        const errors: string[] = [];

        if (!packagings.length) {

            errors.push(
                'At least one packaging is required.'
            );

        }

        const names =
            new Set<string>();

        for (const p of packagings) {

            if (
                !p.name?.trim()
            ) {

                errors.push(
                    'Packaging name is required.'
                );

            }

            if (
                p.unitQuantity <= 0
            ) {

                errors.push(
                    'Packaging units must be greater than zero.'
                );

            }

            const normalized =
                p.name
                    .trim()
                    .toLowerCase();

            if (
                names.has(normalized)
            ) {

                errors.push(
                    `Duplicate packaging: ${p.name}`
                );

            }

            names.add(normalized);

        }

        return errors;

    }

    validatePricing(
        pricing:
            OnboardingPricingDraft[]
    ): string[] {

        const errors: string[] = [];

        for (const row of pricing) {

            if (
                !row.sellingPrice ||
                row.sellingPrice <= 0
            ) {

                errors.push(
                    'Selling price must be greater than zero.'
                );

            }

        }

        return errors;

    }

    validateSuppliers(
        suppliers:
            OnboardingSupplierEntry[]
    ): string[] {

        const errors: string[] = [];

        if (!suppliers.length) {

            errors.push(
                'At least one supplier entry is required.'
            );

        }

        for (const row of suppliers) {

            if (
                !row.supplierId &&
                !row.supplierName?.trim()
            ) {

                errors.push(
                    'Supplier is required.'
                );

            }

            if (
                !row.unitsSupplied ||
                row.unitsSupplied <= 0
            ) {

                errors.push(
                    'Supplier quantity must be greater than zero.'
                );

            }

            if (
                !row.unitCost ||
                row.unitCost <= 0
            ) {

                errors.push(
                    'Supplier unit cost must be greater than zero.'
                );

            }

            if (
                row.vatRate == null ||
                row.vatRate < 0
            ) {
                errors.push(
                    'VAT rate cannot be negative.'
                );
            }

            if (
                row.vatRate > 100
            ) {
                errors.push(
                    'VAT rate cannot exceed 100.'
                );
            }

        }

        return errors;

    }

    validateAccountingAndPayments(
        state: StockOnboardingState
    ): string[] {

        const errors: string[] = [];

        if (
            !state.accountingDate
        ) {
            errors.push(
                'Accounting date is required.'
            );
        }

        if (
            state.autoPaySuppliers &&
            !state.suppliers.length
        ) {

            errors.push(
                'Supplier auto payment requires supplier entries.'
            );

        }

        if (
            state.autoPayOperationalExpenses &&
            !state.operationalExpenses.length
        ) {

            errors.push(
                'Expense auto payment requires at least one operational expense.'
            );

        }
        
        for (
            const expense
            of state.operationalExpenses
        ) {

            if (
                !expense.expenseAccountId
            ) {
                errors.push(
                    'Expense account is required.'
                );
            }

            if (
                !expense.description?.trim()
            ) {
                errors.push(
                    'Expense description is required.'
                );
            }

            if (
                !expense.amount ||
                expense.amount <= 0
            ) {
                errors.push(
                    'Expense amount must be greater than zero.'
                );
            }
        }

        if (
            state.autoPaySuppliers
        ) {

            if (
                !state.supplierPaymentMethod
            ) {
                errors.push(
                    'Supplier payment method is required.'
                );
            }

            if (
                !state.fundingAccountId
            ) {
                errors.push(
                    'Funding account is required for supplier auto payment.'
                );
            }
        }

        if (
            state.autoPayOperationalExpenses
        ) {

            if (
                !state.fundingAccountId
            ) {
                errors.push(
                    'Funding account is required for expense auto payment.'
                );
            }
        }

        return errors;
    }

}