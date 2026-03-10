import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../../environments/environment';
import { Observable } from 'rxjs';

import { PlanResponse, PlanCreateRequest } from '../models/plan.model';

@Injectable({ providedIn: 'root' })
export class PlanService {

  private http = inject(HttpClient);

  private base = `${environment.apiUrl}/platform/plans`;

  getPlans(): Observable<PlanResponse[]> {

    return this.http.get<PlanResponse[]>(this.base);

  }

  getPlan(id: string): Observable<PlanResponse> {

    return this.http.get<PlanResponse>(`${this.base}/${id}`);

  }

  createPlan(payload: PlanCreateRequest): Observable<PlanResponse> {

    return this.http.post<PlanResponse>(this.base, payload);

  }

  updatePlan(id: string, payload: PlanCreateRequest): Observable<PlanResponse> {

    return this.http.put<PlanResponse>(`${this.base}/${id}`, payload);

  }

  deletePlan(id: string) {

    return this.http.delete(`${this.base}/${id}`);

  }

}