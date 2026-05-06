import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { environment } from '../../../../../../environments/environment';
import { PageWrapper } from '../../../../../core/models/page-wrapper.model';

import {
  BulkRequest,
  BulkResult
} from '../../../../../shared/models/bulk-import.model';

import {
  SaleBulkRow,
  SaleDTO,
  SaleRequest
} from '../../stock/models/sale.model';

@Injectable({
  providedIn: 'root'
})
export class SalesService {
  private api = environment.apiUrl;

  private endpoints =
    environment.endpoints.sales;

  constructor(
    private http: HttpClient
  ) { }

  list(params: {
    page?: number;
    size?: number;
    status?: string;
    customer?: string;
    branchId?: string;
    from?: string;
    to?: string;
  }) {
    let httpParams = new HttpParams();

    Object.entries(params)
      .forEach(([key, value]) => {
        if (
          value !== undefined &&
          value !== null
        ) {
          httpParams = httpParams.set(
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

  get(id: string) {
    return this.http.get<SaleDTO>(
      this.api +
      this.endpoints.get(id)
    );
  }

  create(payload: SaleRequest) {
    return this.http.post<SaleDTO>(
      this.api +
      this.endpoints.create,
      payload
    );
  }

  update(
    id: string,
    payload: SaleRequest
  ) {
    return this.http.put<SaleDTO>(
      this.api +
      this.endpoints.update(id),
      payload
    );
  }

  deliver(id: string) {
    return this.http.post<SaleDTO>(
      this.api +
      this.endpoints.deliver(id),
      {}
    );
  }

  cancel(id: string) {
    return this.http.post<SaleDTO>(
      this.api +
      this.endpoints.cancel(id),
      {}
    );
  }

  refund(id: string) {
    return this.http.post<SaleDTO>(
      this.api +
      this.endpoints.refund(id),
      {}
    );
  }

  cancelAndRefund(id: string) {
    return this.http.post<SaleDTO>(
      this.api +
      this.endpoints.cancelAndRefund(id),
      {}
    );
  }

  payments(id: string) {
    return this.http.get(
      this.api +
      this.endpoints.payments(id)
    );
  }

  receipt(id: string) {
    return this.http.get<SaleDTO>(
      this.api +
      this.endpoints.receipt(id)
    );
  }

  bulkCreate(
    payload: BulkRequest<SaleBulkRow>
  ) {
    return this.http.post<
      BulkResult<SaleDTO>
    >(
      this.api +
      this.endpoints.bulk.create,
      payload
    );
  }
}