import {
  Injectable,
  inject
} from '@angular/core';

import {
  HttpClient,
  HttpParams
} from '@angular/common/http';

import {
  Observable
} from 'rxjs';

import {
  environment
} from '../../../../../../../environments/environment';

import {
  BranchContextService
} from '../../../../../../core/services/branch-context.service';

import {
  CorporateTaxFiling,
  PageResponse,
  AccountingPeriod,
  TaxStatus,
  TaxSystemState,
  VatFiling,
  CorporateTaxAccrualPreview,
  CorporateTaxOverview,
  VatOverview,
  VatFilingSummary,
  VatFilingDetail,
  VatFilingPreview,
  RecordVatPaymentRequest,
  VatPayment,
  VatRefund,
  VatCreditMovement,
  VatFilingReadiness,
  VatDashboard
} from '../models/tax.models';
import { FundingAccount } from '../../ap/debts/models/funding-account.model';

@Injectable({
  providedIn: 'root'
})
export class TaxService {

  private readonly http =
    inject(HttpClient);

  private readonly branchContext =
    inject(BranchContextService);

  private readonly vatBase =
    `${environment.apiUrl}/tax/vat`;

  private readonly corporateBase =
    `${environment.apiUrl}/tax/corporate`;

  private readonly systemBase =
    `${environment.apiUrl}/tax/system`;

  private readonly periodsBase =
    `${environment.apiUrl}/accounting/periods`;

  private resolveBranchId(
    override?: string
  ): string {

    const branchId =
      override ??
      this.branchContext.currentBranch;

    if (!branchId) {
      throw new Error(
        'Branch not selected'
      );
    }

    return branchId;
  }

  /* ============================================
     STATUS
  ============================================ */

  getStatus(
    branchId?: string
  ): Observable<TaxStatus> {

    return this.http.get<TaxStatus>(
      `${environment.apiUrl}/tax/system/status`,
      {
        params: {
          branchId:
            this.resolveBranchId(
              branchId
            )
        }
      }
    );
  }

  getConfiguration(
    branchId?: string
  ): Observable<TaxSystemState> {

    return this.http.get<TaxSystemState>(
      `${this.systemBase}/configuration`,
      {
        params: {
          branchId:
            this.resolveBranchId(
              branchId
            )
        }
      }
    );
  }

  configure(
    payload: {
      branchId: string;
      vatEnabled: boolean;
      vatRate: number;
      corporateTaxRate: number;

      vatCreditTreatment:
      | 'CARRY_FORWARD'
      | 'REFUND'
      | 'BOTH';
    }
  ) {
    return this.http.post(
      `${this.systemBase}/configure`,
      {},
      {
        params: {
          branchId:
            payload.branchId,

          vatEnabled:
            payload.vatEnabled,

          vatRate:
            payload.vatRate,

          corporateTaxRate:
            payload.corporateTaxRate,

          vatCreditTreatment:
            payload.vatCreditTreatment
        }
      }
    );
  }

  lock(
    branchId?: string
  ) {

    return this.http.post(
      `${this.systemBase}/lock`,
      {},
      {
        params: {
          branchId:
            this.resolveBranchId(
              branchId
            )
        }
      }
    );
  }

  /* ============================================
   TAX PERIODS
============================================ */

  listPeriods(
    page = 0,
    size = 25,
    branchId?: string
  ): Observable<PageResponse<AccountingPeriod>> {
    return this.http.get<
      PageResponse<AccountingPeriod>
    >(
      this.periodsBase,
      {
        params: {
          branchId:
            this.resolveBranchId(
              branchId
            ),
          page,
          size
        }
      }
    );
  }

  getEligibleVatPeriods(
    branchId?: string
  ): Observable<AccountingPeriod[]> {
    return this.http.get<
      AccountingPeriod[]
    >(
      `${this.periodsBase}/eligible-vat-periods`,
      {
        params: {
          branchId:
            this.resolveBranchId(
              branchId
            )
        }
      }
    );
  }

  getEligibleCorporateTaxPeriods(
    branchId?: string
  ): Observable<AccountingPeriod[]> {

    return this.http.get<
      AccountingPeriod[]
    >(
      `${this.periodsBase}/eligible-corporate-tax-periods`,
      {
        params: {
          branchId:
            this.resolveBranchId(
              branchId
            )
        }
      }
    );
  }

  getCurrentPeriod(
    branchId?: string
  ): Observable<AccountingPeriod | null> {
    return this.http.get<AccountingPeriod>(
      `${this.periodsBase}/current`,
      {
        params: {
          branchId:
            this.resolveBranchId(
              branchId
            )
        }
      }
    );
  }

  closePeriod(
    periodId: string
  ) {

    return this.http.post(
      `${this.periodsBase}/${periodId}/close`,
      {}
    );
  }

  /* ============================================
     VAT
  ============================================ */

  getVatDashboard(
    branchId?: string
  ): Observable<VatDashboard> {
    return this.http.get<VatDashboard>(
      `${this.vatBase}/dashboard`,
      {
        params: {
          branchId:
            this.resolveBranchId(branchId)
        }
      }
    );
  }

  getVatOverview(
    branchId?: string
  ): Observable<VatOverview> {
    return this.http.get<VatOverview>(
      `${this.vatBase}/overview`,
      {
        params: {
          branchId:
            this.resolveBranchId(branchId)
        }
      }
    );
  }

  getVatHistory(
    branchId?: string
  ): Observable<VatFilingSummary[]> {
    return this.http.get<VatFilingSummary[]>(
      `${this.vatBase}/history`,
      {
        params: {
          branchId:
            this.resolveBranchId(branchId)
        }
      }
    );
  }

  getVatDetail(
    filingId: string
  ): Observable<VatFilingDetail> {
    return this.http.get<VatFilingDetail>(
      `${this.vatBase}/detail/${filingId}`
    );
  }

  getVatPreview(
    periodId: string,
    branchId?: string
  ): Observable<VatFilingPreview> {
    return this.http.get<VatFilingPreview>(
      `${this.vatBase}/preview/${periodId}`,
      {
        params: {
          branchId:
            this.resolveBranchId(branchId)
        }
      }
    );
  }

  getVatReadiness(
    periodId: string,
    branchId?: string
  ): Observable<VatFilingReadiness> {
    return this.http.get<VatFilingReadiness>(
      `${this.vatBase}/readiness/${periodId}`,
      {
        params: {
          branchId:
            this.resolveBranchId(branchId)
        }
      }
    );
  }

  fileVat(
    periodId: string,
    branchId?: string
  ) {
    return this.http.post(
      `${this.vatBase}/file/${periodId}`,
      {},
      {
        params: {
          branchId:
            this.resolveBranchId(branchId)
        }
      }
    );
  }

  recordVatPayment(
    filingId: string,
    payload: RecordVatPaymentRequest
  ) {
    return this.http.post(
      `${this.vatBase}/payment/${filingId}`,
      payload
    );
  }

  getVatPayments(
    filingId: string
  ): Observable<VatPayment[]> {
    return this.http.get<VatPayment[]>(
      `${this.vatBase}/payments/${filingId}`
    );
  }

  requestVatRefund(
    filingId: string
  ) {
    return this.http.post(
      `${this.vatBase}/refund/request/${filingId}`,
      {}
    );
  }

  completeVatRefund(
    filingId: string,
    accountId: string
  ) {
    return this.http.post(
      `${this.vatBase}/refund/complete/${filingId}`,
      {},
      {
        params: {
          accountId
        }
      }
    );
  }

  getVatRefunds(
    branchId?: string
  ): Observable<VatRefund[]> {
    return this.http.get<VatRefund[]>(
      `${this.vatBase}/refunds`,
      {
        params: {
          branchId:
            this.resolveBranchId(branchId)
        }
      }
    );
  }

  getVatCreditHistory(
    branchId?: string
  ): Observable<VatCreditMovement[]> {
    return this.http.get<VatCreditMovement[]>(
      `${this.vatBase}/credit-history`,
      {
        params: {
          branchId:
            this.resolveBranchId(branchId)
        }
      }
    );
  }

  /* ============================================
     CORPORATE
  ============================================ */


  getCorporateOverview(
    branchId?: string
  ): Observable<CorporateTaxOverview> {
    return this.http.get<CorporateTaxOverview>(
      `${this.corporateBase}/overview`,
      {
        params: {
          branchId:
            this.resolveBranchId(
              branchId
            )
        }
      }
    );
  }

  getCorporatePreview(
    periodId: string,
    branchId?: string
  ): Observable<CorporateTaxAccrualPreview> {
    return this.http.get<CorporateTaxAccrualPreview>(
      `${this.corporateBase}/accrual-preview/${periodId}`,
      {
        params: {
          branchId:
            this.resolveBranchId(
              branchId
            )
        }
      }
    );
  }
  listCorporate(
    page = 0,
    size = 25,
    branchId?: string
  ): Observable<
    PageResponse<
      CorporateTaxFiling
    >
  > {

    return this.http.get<
      PageResponse<
        CorporateTaxFiling
      >
    >(
      this.corporateBase,
      {
        params: {
          branchId:
            this.resolveBranchId(
              branchId
            ),
          page,
          size
        }
      }
    );
  }

  accrueCorporate(
    periodId: string,
    branchId?: string
  ) {
    return this.http.post(
      `${this.corporateBase}/accrue/${periodId}`,
      {},
      {
        params: {
          branchId: this.resolveBranchId(
            branchId
          )
        }
      }
    );
  }

  payCorporate(
    filingId: string,
    amount: number,
    accountId: string,
    branchId?: string
  ) {
    return this.http.post(
      `${this.corporateBase}/pay/${filingId}`,
      {},
      {
        params: {
          amount,
          accountId,
          branchId:
            this.resolveBranchId(
              branchId
            )
        }
      }
    );
  }

  getFundingAccounts(
    branchId?: string
  ) {
    return this.http.get<
      FundingAccount[]
    >(
      `${environment.apiUrl}/finance/supplier-payments/funding-accounts`,
      {
        params: {
          branchId:
            this.resolveBranchId(
              branchId
            )
        }
      }
    );
  }
}