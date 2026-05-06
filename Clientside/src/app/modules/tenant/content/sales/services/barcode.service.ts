import { Injectable } from '@angular/core';
import { map } from 'rxjs';

import { BaseApiService } from '../../../../../core/services/api/base-api.service';
import { ApiResponse } from '../../../../../core/models/api-response.model';

import { environment } from '../../../../../../environments/environment';

import {
    BarcodeScanRequest,
    BarcodeScanResponse
} from '../../stock/models/barcode.model';

@Injectable({
    providedIn: 'root'
})
export class BarcodeService
    extends BaseApiService {

    scan(
        payload: BarcodeScanRequest
    ) {
        return this.post<
            ApiResponse<BarcodeScanResponse>
        >(
            environment.endpoints.barcodes.scan,
            payload
        ).pipe(
            map(res => this.unwrap(res))
        );
    }

    lookup(barcode: string) {
        return this.get(
            environment.endpoints.barcodes.lookup(
                barcode
            )
        );
    }

    generate(variantId: string) {
        return this.post(
            environment.endpoints.barcodes.generate(
                variantId
            ),
            {}
        );
    }

    image(variantId: string) {
        return this.http.get(
            `${this.api}${environment.endpoints.barcodes.image(variantId)}`,
            {
                responseType: 'blob'
            }
        );
    }
}