import { Injectable } from '@angular/core';

import { map } from 'rxjs';

import { BaseApiService } from '../../../../../../core/services/api/base-api.service';

import { ApiResponse } from '../../../../../../core/models/api-response.model';

import { environment } from '../../../../../../../environments/environment';

import {
    StockOnboardingBulkPreviewResult,
    StockOnboardingRequest,
    StockOnboardingResponse
} from '../../models/stock-onboarding.model';

import {
    BulkRequest,
    BulkResult
} from '../../../../../../shared/models/bulk-import.model';

@Injectable({
    providedIn: 'root'
})
export class StockOnboardingService
    extends BaseApiService {

    create(
        payload: StockOnboardingRequest
    ) {

        return this.post<
            ApiResponse<
                StockOnboardingResponse
            >
        >(
            environment.endpoints.stock
                .onboarding.create,
            payload
        ).pipe(
            map(res => this.unwrap(res))
        );
    }

    bulkCreate(
        payload:
            BulkRequest<
                StockOnboardingRequest
            >
    ) {

        return this.post<
            ApiResponse<
                BulkResult<
                    StockOnboardingBulkPreviewResult
                >
            >
        >(
            environment.endpoints.stock
                .onboarding.bulk,
            payload
        ).pipe(
            map(res => this.unwrap(res))
        );
    }

    previewBulk(
        payload:
            BulkRequest<
                StockOnboardingRequest
            >
    ) {

        return this.bulkCreate({
            ...payload,
            options: {
                ...(payload.options || {}),
                dryRun: true
            }
        });
    }
}