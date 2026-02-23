import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { environment } from '../../../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class TaxService {

  private vatBase = `${environment.apiUrl}/tax/vat`;
  private corporateBase = `${environment.apiUrl}/tax/corporate`;

  constructor(private http: HttpClient) { }

  /* ================= VAT ================= */

  listVat() {
    return this.http.get<any[]>(this.vatBase);
  }

  fileVat(periodId: string) {
    return this.http.post(`${this.vatBase}/file/${periodId}`, {});
  }

  payVat(filingId: string, accountId: string) {
    return this.http.post(
      `${this.vatBase}/pay/${filingId}`,
      {},
      { params: { accountId } }
    );
  }

  /* ================= CORPORATE ================= */

  accrueCorporate(periodId: string, from: string, to: string) {
    return this.http.post(
      `${this.corporateBase}/accrue/${periodId}`,
      {},
      { params: { from, to } }
    );
  }

  payCorporate(filingId: string, accountId: string) {
    return this.http.post(
      `${this.corporateBase}/pay/${filingId}`,
      {},
      { params: { accountId } }
    );
  }

  listCorporate() {
    return this.http.get<any[]>(
      `${environment.apiUrl}/tax/corporate`
    );
  }
}