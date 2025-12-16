import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class InventoryService {

  private base = environment.apiUrl + environment.endpoints.inventory.base;

  constructor(private http: HttpClient) {}

  getVariantAcrossBranches(variantId: string) {
    return this.http.get<any>(
      `${this.base}/variant/${variantId}`
    );
  }
}