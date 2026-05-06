import { Injectable } from '@angular/core';
import { map } from 'rxjs';

import {
    SellableProductRequest,
    SellableProductResponse
} from '../../stock/models/sellable.model';

import { BaseApiService } from '../../../../../core/services/api/base-api.service';
import { ApiResponse } from '../../../../../core/models/api-response.model';

import { environment } from '../../../../../../environments/environment';

@Injectable({
    providedIn: 'root'
})
export class SellableService
    extends BaseApiService {

    search(
        request: SellableProductRequest
    ) {
        return this.post<
            ApiResponse<SellableProductResponse>
        >(
            environment.endpoints.sellable.search,
            request
        ).pipe(
            map(res => this.unwrap(res))
        );
    }

    resolve<T>(request: T) {
        return this.post<
            ApiResponse<unknown>
        >(
            environment.endpoints.sellable.resolve,
            request
        ).pipe(
            map(res => this.unwrap(res))
        );
    }
}