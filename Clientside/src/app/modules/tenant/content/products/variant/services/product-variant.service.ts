import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { map } from 'rxjs';
import { environment } from '../../../../../../../environments/environment';
import { ApiResponse } from '../../../../../../core/models/api-response.model';
import { ProductVariant } from '../../../stock/models/product-variant.model';

@Injectable({
  providedIn: 'root'
})
export class ProductVariantService {

  private api = environment.apiUrl;
  private endpoints = environment.endpoints.products.variants;

  constructor(
    private http: HttpClient
  ) { }

  forProduct(
    productId: string
  ) {
    return this.http
      .get<ApiResponse<ProductVariant[]>>(
        this.api +
        this.endpoints.forProduct(productId)
      )
      .pipe(
        map(res => res.data ?? [])
      );
  }

  get(id: string) {
    return this.http
      .get<ApiResponse<ProductVariant>>(
        this.api +
        this.endpoints.get(id)
      )
      .pipe(
        map(res => res.data as ProductVariant)
      );
  }

  create(
    dto: Partial<ProductVariant>
  ) {
    return this.http
      .post<ApiResponse<ProductVariant>>(
        this.api +
        this.endpoints.create,
        dto
      )
      .pipe(
        map(res => res.data as ProductVariant)
      );
  }

  update(
    id: string,
    dto: Partial<ProductVariant>
  ) {
    return this.http
      .put<ApiResponse<ProductVariant>>(
        this.api +
        this.endpoints.update(id),
        dto
      )
      .pipe(
        map(res => res.data as ProductVariant)
      );
  }

  delete(id: string) {
    return this.http.delete<void>(
      this.api +
      this.endpoints.delete(id)
    );
  }

  getImages(
    variantId: string
  ) {
    return this.http
      .get<ApiResponse<string[]>>(
        this.api +
        this.endpoints.images.list(
          variantId
        )
      )
      .pipe(
        map(res => res.data ?? [])
      );
  }

  uploadImages(
    variantId: string,
    files: File[]
  ) {
    const formData =
      new FormData();

    files.forEach(file => {
      formData.append(
        'files',
        file
      );
    });

    return this.http.post<void>(
      this.api +
      this.endpoints.images.upload(
        variantId
      ),
      formData
    );
  }

  getImageBlob(
    variantId: string,
    fileName: string
  ) {
    return this.http.get(
      this.api +
      this.endpoints.images.image(
        variantId,
        fileName
      ),
      {
        responseType: 'blob'
      }
    );
  }

  downloadImagesZip(
    variantId: string
  ) {
    return this.http.get(
      this.api +
      this.endpoints.images.zip(
        variantId
      ),
      {
        responseType: 'blob'
      }
    );
  }

  generateBarcode(
    variantId: string
  ) {
    return this.http
      .post<ApiResponse<string>>(
        this.api +
        environment.endpoints
          .barcodes
          .generate(variantId),
        {}
      )
      .pipe(
        map(res => res.data)
      );
  }
}