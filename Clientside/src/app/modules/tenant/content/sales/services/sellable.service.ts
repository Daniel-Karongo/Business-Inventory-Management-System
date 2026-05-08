import { Injectable } from '@angular/core';
import { map, Observable } from 'rxjs';

import { environment } from '../../../../../../environments/environment';

import { ApiResponse } from '../../../../../core/models/api-response.model';
import { BaseApiService } from '../../../../../core/services/api/base-api.service';

import {
    SellableProductRequest,
    SellableProductResponse,
    SellableResolveRequest,
    SellableResolveResponse
} from '../../stock/models/sellable.model';

@Injectable({
    providedIn: 'root'
})
export class SellableService extends BaseApiService {

    private readonly endpoints =
        environment.endpoints.sellable;

    search(
        request: SellableProductRequest
    ): Observable<SellableProductResponse> {

        return this.post<
            ApiResponse<SellableProductResponse>
        >(
            this.endpoints.search,
            request
        ).pipe(
            map(response =>
                this.unwrap<SellableProductResponse>(
                    response
                )
            )
        );
    }

    resolve(
        request: SellableResolveRequest
    ): Observable<SellableResolveResponse> {

        return this.post<
            ApiResponse<SellableResolveResponse>
        >(
            this.endpoints.resolve,
            request
        ).pipe(
            map(response =>
                this.unwrap<SellableResolveResponse>(
                    response
                )
            )
        );
    }
}