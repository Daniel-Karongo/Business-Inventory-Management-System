import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { environment } from '../../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class PaymentsService {

  private base = environment.apiUrl + environment.endpoints.payments.base;

  constructor(private http: HttpClient) {}

  create(payload: any) {
    return this.http.post<any>(this.base, payload);
  }

  refund(id: string) {
    return this.http.post<any>(
      environment.apiUrl + environment.endpoints.payments.refund(id),
      {}
    );
  }

  reverse(id: string, note?: string) {
    let params = new HttpParams();
    if (note) params = params.set('note', note);

    return this.http.post<any>(
      environment.apiUrl + environment.endpoints.payments.reverse(id),
      {},
      { params }
    );
  }

  initiateMpesa(saleId: string, phone: string, amount: number) {
    return this.http.post<any>(
      environment.apiUrl + environment.endpoints.payments.mpesa.initiateStk,
      null,
      { params: { saleId, phone, amount } }
    );
  }
}