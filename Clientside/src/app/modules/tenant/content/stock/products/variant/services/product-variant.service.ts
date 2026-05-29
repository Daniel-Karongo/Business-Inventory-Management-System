import { Injectable, inject } from '@angular/core';

import {
  map,
  Observable
} from 'rxjs';

import { environment }
  from '../../../../../../../../environments/environment';

import { ApiResponse }
  from '../../../../../../../core/models/api-response.model';

import { BaseApiService }
  from '../../../../../../../core/services/api/base-api.service';

import { BranchContextService }
  from '../../../../../../../core/services/branch-context.service';

import {
  ProductVariant,
  ProductVariantCreateDTO,
  ProductVariantUpdateDTO
} from '../../../models/product-variant.model';

@Injectable({ providedIn: 'root' })
export class ProductVariantService
  extends BaseApiService {

  private branchContext =
    inject(BranchContextService);

  private endpoints =
    environment.endpoints.products.variants;

  private barcodeEndpoints =
    environment.endpoints.barcodes;

  private resolveBranch(
    override?: string
  ): string {

    const branchId =
      override ??
      this.branchContext.currentBranch;

    if (!branchId) {
      throw new Error(
        'Branch not selected'
      );
    }

    return branchId;
  }

  /* =========================================================
     GET
  ========================================================= */

  getById(
    id: string,
    overrideBranchId?: string
  ): Observable<ProductVariant> {

    return super.get<ProductVariant>(
      this.endpoints.get(id),
      {
        branchId:
          this.resolveBranch(
            overrideBranchId
          )
      }
    );
  }

  forProduct(
    productId: string,
    overrideBranchId?: string
  ): Observable<ProductVariant[]> {

    return super.get<ProductVariant[]>(
      this.endpoints.forProduct(productId),
      {
        branchId:
          this.resolveBranch(
            overrideBranchId
          )
      }
    );
  }

  /* =========================================================
     CREATE
  ========================================================= */

  create(
    payload: ProductVariantCreateDTO,
    overrideBranchId?: string
  ): Observable<ProductVariant> {
    return super.post<ProductVariant>(
      this.endpoints.create,
      payload,
      {
        branchId:
          this.resolveBranch(
            overrideBranchId
          )
      }
    );
  }

  /* =========================================================
     UPDATE
  ========================================================= */

  update(
    id: string,
    payload: ProductVariantUpdateDTO,
    overrideBranchId?: string
  ): Observable<ProductVariant> {
    return super.put<ProductVariant>(
      this.endpoints.update(id),
      payload,
      {
        branchId:
          this.resolveBranch(
            overrideBranchId
          )
      }
    );
  }

  /* =========================================================
     DELETE
  ========================================================= */

  remove(
    id: string,
    overrideBranchId?: string
  ): Observable<void> {

    return super.delete<void>(
      this.endpoints.delete(id),
      {
        branchId:
          this.resolveBranch(
            overrideBranchId
          )
      }
    );
  }

  /* =========================================================
     IMAGES
  ========================================================= */

  getImages(
    variantId: string,
    overrideBranchId?: string
  ): Observable<string[]> {

    return super.get<string[]>(
      this.endpoints.images.list(variantId),
      {
        branchId:
          this.resolveBranch(
            overrideBranchId
          )
      }
    );
  }

  uploadImages(
    variantId: string,
    files: File[],
    overrideBranchId?: string
  ): Observable<void> {

    const fd = new FormData();

    files.forEach(file =>
      fd.append('files', file)
    );

    return this.http.post<void>(
      `${this.api}${this.endpoints.images.upload(variantId)}`,
      fd,
      {
        params: this.buildParams({
          branchId:
            this.resolveBranch(
              overrideBranchId
            )
        })
      }
    );
  }

  downloadImagesZip(
    variantId: string,
    overrideBranchId?: string
  ): Observable<Blob> {

    return this.http.get(
      `${this.api}${this.endpoints.images.zip(variantId)}`,
      {
        params: this.buildParams({
          branchId:
            this.resolveBranch(
              overrideBranchId
            )
        }),
        responseType: 'blob'
      }
    );
  }

  getImageBlob(
    variantId: string,
    fileName: string,
    overrideBranchId?: string
  ): Observable<Blob> {

    return this.http.get(
      `${this.api}${this.endpoints.images.image(
        variantId,
        fileName
      )}`,
      {
        params: this.buildParams({
          branchId:
            this.resolveBranch(
              overrideBranchId
            )
        }),
        responseType: 'blob'
      }
    );
  }

  /* =========================================================
     BARCODES
  ========================================================= */

  getBarcodeImage(
    variantId: string,
    overrideBranchId?: string
  ): Observable<Blob> {

    return this.http.get(
      `${this.api}${this.barcodeEndpoints.image(
        variantId
      )}`,
      {
        params: this.buildParams({
          branchId:
            this.resolveBranch(
              overrideBranchId
            )
        }),
        responseType: 'blob'
      }
    );
  }

  generateBulkBarcodePdf(
    variantIds: string[],
    overrideBranchId?: string
  ): Observable<string> {

    return super.post<string>(
      this.endpoints.barcodePdf.bulk,
      variantIds,
      {
        branchId:
          this.resolveBranch(
            overrideBranchId
          )
      }
    );
  }

  generateProductBarcodePdf(
    productId: string,
    overrideBranchId?: string
  ): Observable<string> {

    return super.get<string>(
      this.endpoints.barcodePdf.product(productId),
      {
        branchId:
          this.resolveBranch(
            overrideBranchId
          )
      }
    );
  }

  downloadBarcodePdf(
    fileName: string,
    overrideBranchId?: string
  ): Observable<Blob> {

    return this.http.get(
      `${this.api}${this.endpoints.barcodePdf.download(
        fileName
      )}`,
      {
        params: this.buildParams({
          branchId:
            this.resolveBranch(
              overrideBranchId
            )
        }),
        responseType: 'blob'
      }
    );
  }
}