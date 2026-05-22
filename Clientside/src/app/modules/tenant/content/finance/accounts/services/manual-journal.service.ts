import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { environment } from '../../../../../../../environments/environment';

import { ManualJournalRequest } from '../models/journal.models';

@Injectable({
  providedIn: 'root'
})
export class ManualJournalService {

  private readonly http = inject(HttpClient);

  private readonly baseUrl =
    `${environment.apiUrl}/accounting/manual-journals`;

  post(
    payload: ManualJournalRequest
  ): Observable<void> {

    return this.http.post<void>(
      this.baseUrl,
      payload
    );
  }
}