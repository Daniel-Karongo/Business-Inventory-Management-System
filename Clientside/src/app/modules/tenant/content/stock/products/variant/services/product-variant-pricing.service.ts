import { Injectable, inject } from '@angular/core';

import { environment }
    from '../../../../../../../../environments/environment';

import { BaseApiService }
    from '../../../../../../../core/services/api/base-api.service';

import { BranchContextService }
    from '../../../../../../../core/services/branch-context.service';

import {
    ProductPrice,
    CreatePriceRequest,
    UpdatePriceRequest
} from '../../../models/pricing.model';

@Injectable({ providedIn: 'root' })
export class ProductVariantPricingService
    extends BaseApiService {

    private branchContext =
        inject(BranchContextService);

    private endpoints =
        environment.endpoints.products.variants.pricing;

    private resolveBranch(
        override?: string
    ): string {

        const branchId =
            override ??
            this.branchContext.currentBranch;

        if (!branchId) {
            throw new Error(
                'Branch not selected'
            );
        }

        return branchId;
    }

    getForVariant(
        variantId: string,
        overrideBranchId?: string
    ) {
        return super.get<ProductPrice[]>(
            this.endpoints.forVariant(variantId),
            {
                branchId:
                    this.resolveBranch(
                        overrideBranchId
                    )
            }
        );
    }

    create(
        payload: CreatePriceRequest,
        overrideBranchId?: string
    ) {

        return super.post<ProductPrice>(
            this.endpoints.create,
            {
                ...payload,
                branchId:
                    this.resolveBranch(
                        overrideBranchId ??
                        payload.branchId
                    )
            }
        );
    }

    update(
        id: string,
        payload: UpdatePriceRequest,
        overrideBranchId?: string
    ) {

        return super.put<ProductPrice>(
            this.endpoints.update(id),
            {
                ...payload,
                branchId:
                    this.resolveBranch(
                        overrideBranchId ??
                        payload.branchId
                    )
            }
        );
    }

    remove(
        id: string,
        overrideBranchId?: string
    ) {

        return super.delete<void>(
            this.endpoints.delete(id),
            {
                branchId:
                    this.resolveBranch(
                        overrideBranchId
                    )
            }
        );
    }
}