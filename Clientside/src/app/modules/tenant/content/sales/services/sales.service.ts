import {
  HttpClient,
  HttpParams
} from '@angular/common/http';

import {
  Injectable
} from '@angular/core';

import {
  Observable
} from 'rxjs';

import {
  environment
} from '../../../../../../environments/environment';

import {
  PageWrapper
} from '../../../../../core/models/page-wrapper.model';

import {
  BulkRequest,
  BulkResult
} from '../../../../../shared/models/bulk-import.model';

import {
  SaleBulkPreviewRow,
  SaleBulkRow,
  SaleDTO,
  SaleRequest
} from '../../stock/models/sale.model';

@Injectable({
  providedIn: 'root'
})
export class SalesService {

  private readonly api =
    environment.apiUrl;

  private readonly endpoints =
    environment.endpoints.sales;

  constructor(
    private readonly http: HttpClient
  ) { }

  list(
    params: {
      page?: number;
      size?: number;
      status?: string;
      customer?: string;
      branchId?: string;
      from?: string;
      to?: string;
    }
  ): Observable<PageWrapper<SaleDTO>> {

    let httpParams =
      new HttpParams();

    Object.entries(params)
      .forEach(([key, value]) => {

        if (
          value !== undefined &&
          value !== null
        ) {

          httpParams =
            httpParams.set(
              key,
              String(value)
            );
        }
      });

    return this.http.get<
      PageWrapper<SaleDTO>
    >(
      this.api +
      this.endpoints.list,
      {
        params: httpParams
      }
    );
  }

  get(
    id: string
  ): Observable<SaleDTO> {

    return this.http.get<SaleDTO>(
      this.api +
      this.endpoints.get(id)
    );
  }

  create(
    payload: SaleRequest
  ): Observable<SaleDTO> {

    return this.http.post<SaleDTO>(
      this.api +
      this.endpoints.create,
      payload
    );
  }

  update(
    id: string,
    payload: SaleRequest
  ): Observable<SaleDTO> {

    return this.http.put<SaleDTO>(
      this.api +
      this.endpoints.update(id),
      payload
    );
  }

  deliver(
    id: string
  ): Observable<SaleDTO> {

    return this.http.post<SaleDTO>(
      this.api +
      this.endpoints.deliver(id),
      {}
    );
  }

  cancel(
    id: string
  ): Observable<SaleDTO> {

    return this.http.post<SaleDTO>(
      this.api +
      this.endpoints.cancel(id),
      {}
    );
  }

  refund(
    id: string
  ): Observable<SaleDTO> {

    return this.http.post<SaleDTO>(
      this.api +
      this.endpoints.refund(id),
      {}
    );
  }

  cancelAndRefund(
    id: string
  ): Observable<SaleDTO> {

    return this.http.post<SaleDTO>(
      this.api +
      this.endpoints.cancelAndRefund(id),
      {}
    );
  }

  payments(
    id: string
  ): Observable<unknown> {

    return this.http.get(
      this.api +
      this.endpoints.payments(id)
    );
  }

  receipt(
    id: string
  ): Observable<SaleDTO> {

    return this.http.get<SaleDTO>(
      this.api +
      this.endpoints.receipt(id)
    );
  }

  import(
    mode:
      'OPERATIONAL' |
      'HISTORICAL',
    payload:
      BulkRequest<SaleBulkRow>
  ): Observable<
    BulkResult<SaleBulkPreviewRow>
  > {

    return this.http.post<
      BulkResult<SaleBulkPreviewRow>
    >(
      `${this.api}/sales/import`,
      payload,
      {
        params: {
          mode
        }
      }
    );
  }
}