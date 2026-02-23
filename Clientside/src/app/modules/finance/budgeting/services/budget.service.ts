import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { environment } from '../../../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class BudgetService {

  private base = `${environment.apiUrl}/budgets`;

  constructor(private http: HttpClient) { }

  get(branchId: string | null, fiscalYear: number) {
    return this.http.get<any>(this.base, {
      params: {
        branchId: branchId ?? '',
        fiscalYear
      }
    });
  }

  create(branchId: string | null, fiscalYear: number) {
    return this.http.post<any>(this.base, null, {
      params: {
        branchId: branchId ?? '',
        fiscalYear
      }
    });
  }

  updateLine(
    budgetId: string,
    accountId: string,
    month: number,
    amount: number
  ) {
    return this.http.post(
      `${this.base}/${budgetId}/lines`,
      null,
      {
        params: { accountId, month, amount }
      }
    );
  }

  branchVariance(branchId: string | null, year: number, month: number, accountId: string) {

    if (!branchId) {
      throw new Error('branchId is required for branch variance');
    }

    return this.http.get(`${this.base}/variance`, {
      params: { branchId, year, month, accountId }
    });
  }

  corporateVariance(year: number, month: number, accountId: string) {
    return this.http.get(`${this.base}/consolidated`, {
      params: { year, month, accountId }
    });
  }

  compareBranches(year: number, month: number, accountId: string) {
    return this.http.get(`${this.base}/compare`, {
      params: { year, month, accountId }
    });
  }
}