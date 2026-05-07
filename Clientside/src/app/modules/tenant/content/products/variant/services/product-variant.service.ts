import { Injectable } from '@angular/core';
import { map, Observable } from 'rxjs';

import { environment } from '../../../../../../../environments/environment';

import { ApiResponse } from '../../../../../../core/models/api-response.model';
import { BaseApiService } from '../../../../../../core/services/api/base-api.service';

import {
  ProductVariant,
  ProductVariantCreateDTO,
  ProductVariantUpdateDTO
} from '../../../stock/models/product-variant.model';

@Injectable({ providedIn: 'root' })
export class ProductVariantService extends BaseApiService {

  private endpoints = environment.endpoints.products.variants;
  private barcodeEndpoints = environment.endpoints.barcodes;

  /* =========================================================
     GET
  ========================================================= */

  getById(id: string): Observable<ProductVariant> {
    return super
      .get<ApiResponse<ProductVariant>>(this.endpoints.get(id))
      .pipe(map(res => this.unwrap<ProductVariant>(res)!));
  }

  forProduct(productId: string): Observable<ProductVariant[]> {
    return super
      .get<ApiResponse<ProductVariant[]>>(this.endpoints.forProduct(productId))
      .pipe(map(res => this.unwrap<ProductVariant[]>(res) ?? []));
  }

  /* =========================================================
     CREATE
  ========================================================= */

  create(payload: ProductVariantCreateDTO): Observable<ProductVariant> {
    return super
      .post<ApiResponse<ProductVariant>>(this.endpoints.create, payload)
      .pipe(map(res => this.unwrap<ProductVariant>(res)!));
  }

  /* =========================================================
     UPDATE
  ========================================================= */

  update(id: string, payload: ProductVariantUpdateDTO): Observable<ProductVariant> {
    return super
      .put<ApiResponse<ProductVariant>>(this.endpoints.update(id), payload)
      .pipe(map(res => this.unwrap<ProductVariant>(res)!));
  }

  /* =========================================================
     DELETE
  ========================================================= */

  remove(id: string): Observable<void> {
    return super.delete<void>(
      this.endpoints.delete(id)
    );
  }

  /* =========================================================
     IMAGES
  ========================================================= */

  getImages(variantId: string): Observable<string[]> {
    return super.get<string[]>(this.endpoints.images.list(variantId));
  }

  uploadImages(variantId: string, files: File[]): Observable<void> {
    const fd = new FormData();

    files.forEach(file => fd.append('files', file));

    return this.http.post<void>(
      `${this.api}${this.endpoints.images.upload(variantId)}`,
      fd
    );
  }

  downloadImagesZip(variantId: string): Observable<Blob> {
    return this.http.get(
      `${this.api}${this.endpoints.images.zip(variantId)}`,
      { responseType: 'blob' }
    );
  }

  getImageBlob(variantId: string, fileName: string): Observable<Blob> {
    return this.http.get(
      `${this.api}${this.endpoints.images.image(variantId, fileName)}`,
      { responseType: 'blob' }
    );
  }

  /* =========================================================
     BARCODES
  ========================================================= */

  getBarcodeImage(variantId: string): Observable<Blob> {
    return this.http.get(
      `${this.api}${this.barcodeEndpoints.image(variantId)}`,
      { responseType: 'blob' }
    );
  }

  generateBulkBarcodePdf(variantIds: string[]): Observable<string> {
    return super.post<string>(
      this.endpoints.barcodePdf.bulk,
      variantIds
    );
  }

  generateProductBarcodePdf(productId: string): Observable<string> {
    return super.get<string>(
      this.endpoints.barcodePdf.product(productId)
    );
  }

  downloadBarcodePdf(fileName: string): Observable<Blob> {
    return this.http.get(
      `${this.api}${this.endpoints.barcodePdf.download(fileName)}`,
      { responseType: 'blob' }
    );
  }
}