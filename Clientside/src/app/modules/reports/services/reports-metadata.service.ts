import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../environments/environment';
import { Observable } from 'rxjs';
import { ReportMetadata } from '../models/report-metadata.model';

@Injectable({ providedIn: 'root' })
export class ReportsMetadataService {

  private base = environment.apiUrl + '/reports';

  constructor(private http: HttpClient) {}

  list(): Observable<ReportMetadata[]> {
    return this.http.get<ReportMetadata[]>(`${this.base}/definitions`);
  }
}