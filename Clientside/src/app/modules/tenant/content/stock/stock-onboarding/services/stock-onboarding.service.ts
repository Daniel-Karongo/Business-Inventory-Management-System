import { Injectable } from '@angular/core';

import { map } from 'rxjs';

import { BaseApiService } from '../../../../../../core/services/api/base-api.service';

import { ApiResponse } from '../../../../../../core/models/api-response.model';

import { environment } from '../../../../../../../environments/environment';

import {
    StockOnboardingRequest,
    StockOnboardingResponse
} from '../../models/stock-onboarding.model';

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

}