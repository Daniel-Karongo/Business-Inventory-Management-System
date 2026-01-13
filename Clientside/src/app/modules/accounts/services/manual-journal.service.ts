import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { environment } from '../../../../environments/environment';

export interface ManualJournalLine {
  accountId: string;
  direction: 'DEBIT' | 'CREDIT';
  amount: number;
}

export interface ManualJournalRequest {
  reference: string;
  description?: string;
  lines: ManualJournalLine[];
}

@Injectable({ providedIn: 'root' })
export class ManualJournalService {

  private base =
    environment.apiUrl + environment.endpoints.manualJournals.base;

  constructor(private http: HttpClient) {}

  post(req: ManualJournalRequest) {
    return this.http.post<void>(this.base, req);
  }
}