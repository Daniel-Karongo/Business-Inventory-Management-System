import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { environment } from '../../../../environments/environment';
import { Observable } from 'rxjs';

@Injectable({ providedIn: 'root' })
export class ReportsService {

  private base = environment.apiUrl + '/reports';

  constructor(private http: HttpClient) { }

  generate(reportName: string, parameters: any, format: string) {

    return this.http.post(
      `${this.base}/generate`,
      { reportName, parameters, format },
      { responseType: 'blob' }
    );
  }
}