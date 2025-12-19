import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class SmsService {

  private http = inject(HttpClient);
  private base = `${environment.apiUrl}/notification/sms`;

  sendToCustomers(customerIds: string[], message: string) {
    return this.http.post<any>(`${this.base}/send-to-customers`, {
      customerIds,
      message
    });
  }
}