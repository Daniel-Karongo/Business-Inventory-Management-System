import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../../environments/environment';
import { ApiResponse } from '../../core/models/api-response.model';
import { DashboardSummary } from './models/dashboard-summary.model';

@Injectable({ providedIn: 'root' })
export class DashboardService {

  constructor(private http: HttpClient) {}

  getSummary(branchId: string): Observable<ApiResponse<DashboardSummary>> {
    return this.http.get<ApiResponse<DashboardSummary>>(
      `${environment.apiUrl}/dashboard/summary`,
      { params: { branchId } }
    );
  }
}