import { Injectable } from '@angular/core';
import { map, Observable } from 'rxjs';
import {
    SaleLinePreviewRequest,
    SaleLinePreviewResponse
} from '../../stock/models/sale-preview.model';
import { BaseApiService } from '../../../../../core/services/api/base-api.service';
import { ApiResponse } from '../../../../../core/models/api-response.model';
import { environment } from '../../../../../../environments/environment';

@Injectable({
    providedIn: 'root'
})
export class SalePreviewService
    extends BaseApiService {

    previewLine(
        request: SaleLinePreviewRequest
    ): Observable<SaleLinePreviewResponse> {

        return this.post<
            ApiResponse<SaleLinePreviewResponse>
        >(
            environment.endpoints.sales.previewLine,
            request
        ).pipe(
            map(res => this.unwrap(res))
        );
    }
}