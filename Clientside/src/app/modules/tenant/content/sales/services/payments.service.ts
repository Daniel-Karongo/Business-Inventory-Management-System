import {
  HttpClient,
  HttpParams
} from '@angular/common/http';

import { Injectable } from '@angular/core';

import { Observable } from 'rxjs';

import { environment } from '../../../../../../environments/environment';

import {
  PaymentDTO
} from '../../stock/models/sale.model';
import { BranchContextService } from '../../../../../core/services/branch-context.service';

@Injectable({
  providedIn: 'root'
})
export class PaymentsService {

  private readonly api =
    environment.apiUrl;

  private readonly endpoints =
    environment.endpoints.payments;

  constructor(
    private readonly http: HttpClient,
    private readonly branchContext: BranchContextService
  ) { }

  private branchId(): string {

    const branchId =
      this.branchContext.currentBranch;

    if (!branchId) {
      throw new Error(
        'No active branch selected.'
      );
    }

    return branchId;
  }

  create(
    payload: unknown
  ): Observable<unknown> {

    return this.http.post(
      this.api +
      this.endpoints.create(
        this.branchId()
      ),
      payload
    );
  }

  refund(
    id: string
  ): Observable<unknown> {

    return this.http.post(
      this.api +
      this.endpoints.refund(
        this.branchId(),
        id
      ),
      {}
    );
  }

  reverse(
    id: string,
    note?: string
  ): Observable<unknown> {

    let params =
      new HttpParams();

    if (note) {
      params =
        params.set('note', note);
    }

    return this.http.post(
      this.api +
      this.endpoints.reverse(
        this.branchId(),
        id
      ),
      {},
      { params }
    );
  }

  initiateMpesa(
    saleId: string,
    phone: string,
    amount: number
  ): Observable<unknown> {

    return this.http.post(
      this.api +
      this.endpoints.mpesa
        .initiateStk(
          this.branchId()
        ),
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

  getBySale(
    saleId: string
  ): Observable<PaymentDTO[]> {

    return this.http.get<PaymentDTO[]>(
      this.api +
      environment.endpoints.sales.payments(
        saleId
      )
    );
  }
}