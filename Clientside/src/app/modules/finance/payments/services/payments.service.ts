import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class PaymentsService {

  private base = `${environment.apiUrl}/finance`;

  constructor(private http: HttpClient) {}

  paySupplier(
    supplierId: string,
    amount: number,
    method: string,
    reference?: string
  ) {

    const params: any = { supplierId, amount, method };

    if (reference) {
      params.reference = reference;
    }

    return this.http.post(
      `${this.base}/supplier-payments`,
      null,
      { params }
    );
  }

  listSupplierPayments(page = 0, size = 20) {
    return this.http.get<any>(
      `${this.base}/supplier-payments`,
      { params: { page, size } }
    );
  }
}