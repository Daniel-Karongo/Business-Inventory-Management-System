import { Injectable } from '@angular/core';

import { map, Observable } from 'rxjs';

import { environment } from '../../../../../../environments/environment';

import { ApiResponse } from '../../../../../core/models/api-response.model';
import { BaseApiService } from '../../../../../core/services/api/base-api.service';

import {
    SaleLinePreviewRequest,
    SaleLinePreviewResponse
} from '../../stock/models/sale-preview.model';

@Injectable({
    providedIn: 'root'
})
export class SalePreviewService extends BaseApiService {

    private readonly endpoints =
        environment.endpoints.sales;

    previewLine(
        request: SaleLinePreviewRequest
    ): Observable<SaleLinePreviewResponse> {

        return this.post<
            ApiResponse<SaleLinePreviewResponse>
        >(
            this.endpoints.previewLine,
            request
        ).pipe(
            map(response =>
                this.unwrap<SaleLinePreviewResponse>(
                    response
                )
            )
        );
    }
}