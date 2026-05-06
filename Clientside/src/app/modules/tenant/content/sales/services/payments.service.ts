import { Injectable } from '@angular/core';
import {
  HttpClient,
  HttpParams
} from '@angular/common/http';

import { environment } from '../../../../../../environments/environment';

@Injectable({
  providedIn: 'root'
})
export class PaymentsService {
  private api = environment.apiUrl;

  private endpoints =
    environment.endpoints.payments;

  constructor(
    private http: HttpClient
  ) { }

  create(payload: unknown) {
    return this.http.post(
      this.api +
      this.endpoints.create,
      payload
    );
  }

  refund(id: string) {
    return this.http.post(
      this.api +
      this.endpoints.refund(id),
      {}
    );
  }

  reverse(
    id: string,
    note?: string
  ) {
    let params = new HttpParams();

    if (note) {
      params = params.set(
        'note',
        note
      );
    }

    return this.http.post(
      this.api +
      this.endpoints.reverse(id),
      {},
      { params }
    );
  }

  initiateMpesa(
    saleId: string,
    phone: string,
    amount: number
  ) {
    return this.http.post(
      this.api +
      this.endpoints.mpesa.initiateStk,
      null,
      {
        params: {
          saleId,
          phone,
          amount
        }
      }
    );
  }

  getBySale(saleId: string) {
    return this.http.get(
      this.api +
      environment.endpoints.sales.payments(
        saleId
      )
    );
  }
}