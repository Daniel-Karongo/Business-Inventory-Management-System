import { Injectable } from '@angular/core';

import {
    OnboardingPackagingDraft,
    OnboardingPricingDraft,
    OnboardingSupplierEntry
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
                !row.quantity ||
                row.quantity <= 0
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

        }

        return errors;

    }

}