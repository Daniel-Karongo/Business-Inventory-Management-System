import { InventoryResponse } from './inventory-response.model';
import { PackagingDTO } from './packaging.model';
import { ProductPrice } from './pricing.model';
import { ProductVariant } from './product-variant.model';

export interface VariantCommercialSummary {
    packagingCount: number;
    pricingCount: number;
    totalStock: number;
    hasBasePackaging: boolean;
}

export interface VariantCommercialView {
    variant: ProductVariant;

    inventory?: InventoryResponse[];

    packagings: PackagingDTO[];

    pricingByPackaging: Record<string, ProductPrice[]>;

    warnings: string[];

    summary: VariantCommercialSummary;
}