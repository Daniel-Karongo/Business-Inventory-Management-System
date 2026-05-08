import { Injectable } from '@angular/core';

import { map, Observable } from 'rxjs';

import { environment } from '../../../../../../environments/environment';

import { ApiResponse } from '../../../../../core/models/api-response.model';
import { BaseApiService } from '../../../../../core/services/api/base-api.service';

import {
    BarcodeScanRequest,
    BarcodeScanResponse
} from '../../stock/models/barcode.model';

@Injectable({
    providedIn: 'root'
})
export class BarcodeService extends BaseApiService {

    private readonly endpoints =
        environment.endpoints.barcodes;

    scan(
        payload: BarcodeScanRequest
    ): Observable<BarcodeScanResponse> {

        return this.post<
            ApiResponse<BarcodeScanResponse>
        >(
            this.endpoints.scan,
            payload
        ).pipe(
            map(response =>
                this.unwrap<BarcodeScanResponse>(
                    response
                )
            )
        );
    }

    lookup(
        barcode: string
    ): Observable<unknown> {

        return this.get<
            ApiResponse<unknown>
        >(
            this.endpoints.lookup(barcode)
        ).pipe(
            map(response =>
                this.unwrap(response)
            )
        );
    }

    generate(
        variantId: string
    ): Observable<string> {

        return this.post<
            ApiResponse<string>
        >(
            this.endpoints.generate(
                variantId
            ),
            {}
        ).pipe(
            map(response =>
                this.unwrap<string>(
                    response
                )
            )
        );
    }

    image(
        variantId: string
    ) {

        return this.http.get(
            `${this.api}${this.endpoints.image(
                variantId
            )}`,
            {
                responseType: 'blob'
            }
        );
    }
}