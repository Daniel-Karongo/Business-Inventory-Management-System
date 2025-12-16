import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../../environments/environment';
import { Product } from '../models/product.model';
import { ProductVariant } from '../../variant/models/product-variant.model';

@Injectable({ providedIn: 'root' })
export class ProductService {

  private base = environment.apiUrl + environment.endpoints.products.base;

  constructor(private http: HttpClient) { }

  /* =======================
     PRODUCTS
  ======================= */

  getAll(deleted?: boolean) {
    return this.http.get<Product[]>(
      `${this.base}${deleted !== undefined ? `?deleted=${deleted}` : ''}`
    );
  }

  getAdvanced(params: any) {
    return this.http.get<any>(`${this.base}/advanced`, { params });
  }

  getById(id: string, deleted?: boolean) {
    return this.http.get<Product>(
      `${this.base}/${id}${deleted !== undefined ? `?deleted=${deleted}` : ''}`
    );
  }

  create(formData: FormData) {
    return this.http.post<Product>(`${this.base}/create`, formData);
  }

  bulkCreate(formData: FormData) {
    return this.http.post<Product[]>(
      `${this.base}/create/bulk`,
      formData
    );
  }

  bulkSoftDelete(ids: string[]) {
    return this.http.delete(`${this.base}/soft/bulk`, { body: ids });
  }

  bulkRestore(ids: string[]) {
    return this.http.put(`${this.base}/restore/bulk`, ids);
  }

  bulkHardDelete(ids: string[]) {
    return this.http.delete(`${this.base}/hard/bulk`, { body: ids });
  }

  update(id: string, payload: any) {
    return this.http.patch<Product>(`${this.base}/${id}`, payload);
  }

  softDelete(id: string) {
    return this.http.delete(`${this.base}/soft/${id}`);
  }

  restore(id: string) {
    return this.http.put(`${this.base}/restore/${id}`, {});
  }

  hardDelete(id: string) {
    return this.http.delete(`${this.base}/hard/${id}`);
  }

  /* =======================
     IMAGES
  ======================= */

  uploadImages(id: string, files: File[]) {
    const fd = new FormData();
    files.forEach(f => fd.append('files', f));
    return this.http.patch(`${this.base}/${id}/images`, fd);
  }

  deleteImage(id: string, filename: string) {
    return this.http.delete(`${this.base}/${id}/images/${filename}`);
  }

  downloadImagesZip(id: string, deleted?: boolean) {
    return this.http.get(
      `${this.base}/${id}/images/zip${deleted !== undefined ? `?deleted=${deleted}` : ''}`,
      { responseType: 'blob' }
    );
  }

  getBarcodeImage(barcode: string) {
    return this.http.get(
      `${this.base}/barcode/image/${barcode}`,
      { responseType: 'blob' }
    );
  }

  getBarcodePdf(barcode: string) {
    return this.http.get(
      `${this.base}/barcode/pdf/${barcode}`,
      { responseType: 'blob' }
    );
  }

  /* =======================
     VARIANTS
  ======================= */

  getVariants(productId: string) {
    return this.http.get<ProductVariant[]>(
      `${environment.apiUrl}${environment.endpoints.products.variants.forProduct(productId)}`
    );
  }

  createVariant(payload: any) {
    return this.http.post<ProductVariant>(
      `${environment.apiUrl}${environment.endpoints.products.variants.create}`,
      payload
    );
  }

  updateVariant(id: string, payload: any) {
    return this.http.put<ProductVariant>(
      `${environment.apiUrl}${environment.endpoints.products.variants.update(id)}`,
      payload
    );
  }

  deleteVariant(id: string) {
    return this.http.delete(
      `${environment.apiUrl}${environment.endpoints.products.variants.delete(id)}`
    );
  }

  /* =======================
   AUDITS
======================= */

  getAudits(productId: string) {
    return this.http.get<any[]>(
      `${environment.apiUrl}${environment.endpoints.products.audits(productId)}`
    );
  }
}