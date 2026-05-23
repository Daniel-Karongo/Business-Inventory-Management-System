import { Injectable } from '@angular/core';

import { Observable } from 'rxjs';

import { environment } from '../../../../../../environments/environment';

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

        return this.http.post<SaleLinePreviewResponse>(
            `${this.api}${this.endpoints.previewLine}`,
            request
        );
    }
}