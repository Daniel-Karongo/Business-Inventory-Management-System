import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';

import { environment } from '../../../../environments/environment';

import {
  Supplier,
  SupplierMinimalDTO,
  SupplierAudit,
  SupplierImage,
  SupplierImageAudit
} from '../models/supplier.model';

import { Product } from '../../products/parent/models/product.model';

@Injectable({ providedIn: 'root' })
export class SupplierService {

  private base = environment.apiUrl + environment.endpoints.suppliers.base;

  constructor(private http: HttpClient) { }

  /* ============================================================
     LIST / GET
  ============================================================ */

  getAll(deleted?: boolean): Observable<Supplier[]> {
    if (deleted !== undefined) {
      let params = new HttpParams();
      params = params.set('deleted', String(deleted));
      return this.http.get<Supplier[]>(`${this.base}/all`, { params }); 
    } else {
      return this.http.get<Supplier[]>(`${this.base}/all`);
    }
  }

  getById(identifier: string, deleted?: boolean): Observable<Supplier> {
    let params = new HttpParams();

    if (deleted !== undefined) {
      params = params.set('deleted', String(deleted));
      return this.http.get<Supplier>(
        `${this.base}/identifier/${identifier}`,
        { params }
      );
    } else {
      return this.http.get<Supplier>(
        `${this.base}/identifier/${identifier}`
      );
    }
  }

  /* ============================================================
     CREATE / UPDATE
  ============================================================ */

  create(payload: FormData): Observable<Supplier> {
    return this.http.post<Supplier>(
      `${this.base}/register`,
      payload
    );
  }

  update(id: string, payload: Partial<Supplier>): Observable<Supplier> {
    return this.http.patch<Supplier>(
      `${this.base}/${id}`,
      payload
    );
  }

  /* ============================================================
     DELETE / RESTORE
  ============================================================ */

  softDelete(id: string, reason: string) {
    return this.http.delete(
      `${this.base}/${id}/soft`,
      { body: reason }
    );
  }

  restore(id: string, reason: string) {
    return this.http.patch(
      `${this.base}/restore/${id}`,
      reason
    );
  }

  hardDelete(id: string) {
    return this.http.delete(
      `${this.base}/${id}/hard`
    );
  }

  /* ============================================================
     IMAGES
  ============================================================ */

  listImages(id: string, deleted?: boolean): Observable<SupplierImage[]> {
    let params = new HttpParams();
    if (deleted !== undefined) {
      params = params.set('deleted', String(deleted));
      return this.http.get<SupplierImage[]>(
        `${this.base}/${id}/images`,
        { params }
      );
    } else {
      return this.http.get<SupplierImage[]>(
        `${this.base}/${id}/images`
      );
    }
  }

  getSupplierImageBlob(
    supplierId: string,
    filename: string,
    deleted?: boolean
  ) {
    let params = new HttpParams();

    if (deleted !== undefined) {
      params = params.set('deleted', String(deleted));
      return this.http.get(
        `${this.base}/${supplierId}/images/${encodeURIComponent(filename)}`,
        {
          params,
          responseType: 'blob'
        }
      );
    } else {
      return this.http.get(
        `${this.base}/${supplierId}/images/${encodeURIComponent(filename)}`,
        {
          responseType: 'blob'
        }
      );
    }
  }

  uploadImages(
    id: string,
    files: { file: File; description?: string }[]
  ) {
    const fd = new FormData();

    files.forEach((f, i) => {
      fd.append(`supplierFiles[${i}].file`, f.file);
      fd.append(`supplierFiles[${i}].description`, f.description ?? '');
    });

    return this.http.patch(
      `${this.base}/${id}/images`,
      fd
    );
  }

  softDeleteImage(id: string, filename: string) {
    return this.http.delete(
      `${this.base}/${id}/images/${filename}`,
      { responseType: 'text' }
    );
  }

  restoreImage(id: string, filename: string) {
    return this.http.patch(
      `${this.base}/${id}/images/${filename}/restore`,
      {},
      { responseType: 'text' }
    );
  }

  hardDeleteImage(id: string, filename: string) {
    return this.http.delete(
      `${this.base}/${id}/images/${filename}/hard`,
      { responseType: 'text' }
    );
  }

  /* ============================================================
     AUDITS
  ============================================================ */

  supplierAudits(id: string): Observable<SupplierAudit[]> {
    return this.http.get<SupplierAudit[]>(
      `${this.base}/${id}/audit`
    );
  }

  imageAudits(id: string): Observable<SupplierImageAudit[]> {
    return this.http.get<SupplierImageAudit[]>(
      `${this.base}/${id}/images/audit`
    );
  }

  /* ============================================================
     PRODUCTS (READ-ONLY)
  ============================================================ */

  productsSupplied(
    supplierId: string,
    deleted = false
  ): Observable<Product[]> {
    return this.http.get<Product[]>(
      `${environment.apiUrl}/products/supplier/${supplierId}`,
      {
        params: new HttpParams().set('deleted', String(deleted))
      }
    );
  }
}
