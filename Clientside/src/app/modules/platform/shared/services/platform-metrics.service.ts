import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class PlatformMetricsService {

  private http = inject(HttpClient);

  private base = `${environment.apiUrl}/platform/metrics`;

  getTotalRequests(): Observable<number> {
    return this.http.get<number>(`${this.base}/requests`);
  }

  getTotalErrors(): Observable<number> {
    return this.http.get<number>(`${this.base}/errors`);
  }

}
