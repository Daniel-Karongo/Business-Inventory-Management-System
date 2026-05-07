import { Injectable } from '@angular/core';
import { map } from 'rxjs';

import { environment }
    from '../../../../../../environments/environment';

import { ApiResponse }
    from '../../../../../core/models/api-response.model';

import { BaseApiService }
    from '../../../../../core/services/api/base-api.service';

import {
    BarcodeScanRequest,
    BarcodeScanResponse
} from '../../stock/models/barcode.model';

@Injectable({
    providedIn: 'root'
})
export class BarcodeService
    extends BaseApiService {

    private endpoints =
        environment.endpoints
            .barcodes;

    scan(
        payload: BarcodeScanRequest
    ) {

        return super.post<
            ApiResponse<BarcodeScanResponse>
        >(
            this.endpoints.scan,
            payload
        ).pipe(
            map(res =>
                this.unwrap<BarcodeScanResponse>(
                    res
                )
            )
        );
    }

    lookup(
        barcode: string
    ) {

        return super.get<
            ApiResponse<any>
        >(
            this.endpoints.lookup(
                barcode
            )
        ).pipe(
            map(res =>
                this.unwrap(res)
            )
        );
    }

    generate(
        variantId: string
    ) {

        return super.post<
            ApiResponse<string>
        >(
            this.endpoints.generate(
                variantId
            ),
            {}
        ).pipe(
            map(res =>
                this.unwrap<string>(
                    res
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