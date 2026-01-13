import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { environment } from '../../../../environments/environment';

export interface JournalEntry {
  id: string;
  reference: string;
  description?: string;
  sourceModule: string;
  postedBy: string;
  postedAt: string;
  reversed: boolean;
  entries: {
    accountId: string;
    accountCode: string;
    accountName: string;
    direction: 'DEBIT' | 'CREDIT';
    amount: number;
  }[];
}

@Injectable({ providedIn: 'root' })
export class JournalService {

  private base = environment.apiUrl + '/accounting/journals';

  constructor(private http: HttpClient) {}

  list() {
    return this.http.get<JournalEntry[]>(this.base);
  }

  get(id: string) {
    return this.http.get<JournalEntry>(`${this.base}/${id}`);
  }

  reverse(id: string, reason: string) {
    return this.http.post<void>(
      `${this.base}/${id}/reverse`,
      { reason }
    );
  }
}