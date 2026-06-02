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
  VatFiling
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
            payload.corporateTaxRate
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

  listVat(
    page = 0,
    size = 25,
    branchId?: string
  ): Observable<
    PageResponse<VatFiling>
  > {

    return this.http.get<
      PageResponse<VatFiling>
    >(
      this.vatBase,
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
            this.resolveBranchId(
              branchId
            )
        }
      }
    );
  }

  payVat(
    filingId: string,
    accountId: string,
    branchId?: string
  ) {

    return this.http.post(
      `${this.vatBase}/pay/${filingId}`,
      {},
      {
        params: {
          branchId:
            this.resolveBranchId(
              branchId
            ),
          accountId
        }
      }
    );
  }

  /* ============================================
     CORPORATE
  ============================================ */

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
    accountId: string,
    branchId?: string
  ) {
    return this.http.post(
      `${this.corporateBase}/pay/${filingId}`,
      {},
      {
        params: {
          accountId,
          branchId: this.resolveBranchId(
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