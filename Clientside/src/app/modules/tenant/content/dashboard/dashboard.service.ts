import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../../../../environments/environment';
import { ApiResponse } from '../../../../core/models/api-response.model';
import { DashboardSummary } from './models/dashboard-summary.model';
import { BranchContextService } from '../../../../core/services/branch-context.service';

@Injectable({ providedIn: 'root' })
export class DashboardService {

  private base =
    `${environment.apiUrl}/dashboard`;

  constructor(
    private http: HttpClient,
    private branchContext: BranchContextService
  ) { }

  private resolveBranch(
    overrideBranchId?: string
  ): string {

    const branchId =
      overrideBranchId ??
      this.branchContext.currentBranch;

    if (!branchId) {
      throw new Error('Branch not selected');
    }

    return branchId;
  }

  getSummary(
    overrideBranchId?: string
  ): Observable<ApiResponse<DashboardSummary>> {

    const branchId =
      this.resolveBranch(overrideBranchId);

    return this.http.get<ApiResponse<DashboardSummary>>(
      `${this.base}/summary`,
      {
        params: {
          branchId
        }
      }
    );
  }
}