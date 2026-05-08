import { Injectable } from '@angular/core';

import { environment } from '../../../../../../../../environments/environment';

import { BaseApiService } from '../../../../../../../core/services/api/base-api.service';

import {
    ProductPrice,
    CreatePriceRequest,
    UpdatePriceRequest
} from '../../../models/pricing.model';

@Injectable({ providedIn: 'root' })
export class ProductVariantPricingService extends BaseApiService {

    private endpoints = environment.endpoints.products.variants.pricing;

    getForVariant(variantId: string) {
        return super.get<ProductPrice[]>(
            this.endpoints.forVariant(variantId)
        );
    }

    create(payload: CreatePriceRequest) {
        return super.post<ProductPrice>(
            this.endpoints.create,
            payload
        );
    }

    update(id: string, payload: UpdatePriceRequest) {
        return super.put<ProductPrice>(
            this.endpoints.update(id),
            payload
        );
    }

    remove(id: string) {
        return super.delete<void>(
            this.endpoints.delete(id)
        );
    }
}