import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { environment } from '../../../../environments/environment';
import { Supplier } from '../models/supplier.model';

@Injectable({
  providedIn: 'root'
})
export class SupplierService {

  private base = environment.apiUrl + environment.endpoints.suppliers.base;

  constructor(
    private http: HttpClient
  ) { }

  getAll(deleted: any) {
      return this.http.get<Supplier[]>(
        deleted === undefined ? `${this.base}/all`
        : `${this.base}/all?deleted=${deleted}`
      );
    }
}
