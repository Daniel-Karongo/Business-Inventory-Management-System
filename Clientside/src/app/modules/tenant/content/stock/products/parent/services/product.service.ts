import { Injectable, inject } from '@angular/core';
import { map } from 'rxjs';
import { environment } from '../../../../../../../../environments/environment';
import { ApiResponse } from '../../../../../../../core/models/api-response.model';
import { PageWrapper } from '../../../../../../../core/models/page-wrapper.model';
import { BaseApiService } from '../../../../../../../core/services/api/base-api.service';
import { BranchContextService } from '../../../../../../../core/services/branch-context.service';
import { BulkResult } from '../../../../../../../shared/models/bulk-import.model';
import {
  Product,
  ProductCreateDTO,
  ProductImage,
  ProductImageAudit,
  ProductUpdateDTO
} from '../../../models/product.model';

@Injectable({ providedIn: 'root' })
export class ProductService extends BaseApiService {

  private branchContext = inject(BranchContextService);
  private endpoints = environment.endpoints.products;

  private resolveBranch(override?: string): string {
    const branchId = override ?? this.branchContext.currentBranch;

    if (!branchId) {
      throw new Error('Branch not selected');
    }

    return branchId;
  }

  /* ================= GET ================= */

  getAll(
    deleted?: boolean,
    overrideBranchId?: string
  ) {
    return this.get<ApiResponse<Product[]>>(
      this.endpoints.list,
      {
        branchId: this.resolveBranch(overrideBranchId),
        deleted
      }
    ).pipe(
      map(res => this.unwrap<Product[]>(res) ?? [])
    );
  }

  search(params: {
    branchId?: string;
    categoryIds?: number[];
    categoryId?: number;
    name?: string;
    description?: string;
    keyword?: string;
    minPrice?: number;
    maxPrice?: number;
    deleted?: boolean;
    includeDeleted?: boolean;
    page?: number;
    size?: number;
    sortBy?: string;
    direction?: 'asc' | 'desc';
    minSuppliers?: number;
    maxSuppliers?: number;
    supplierId?: string;
  }) {
    return this.get<PageWrapper<Product>>(
      this.endpoints.search.base,
      {
        ...params,
        branchId: this.resolveBranch(params.branchId)
      }
    );
  }

  getById(
    id: string,
    deleted?: boolean,
    overrideBranchId?: string
  ) {
    return this.get<ApiResponse<Product>>(
      this.endpoints.get(id),
      {
        branchId: this.resolveBranch(overrideBranchId),
        deleted
      }
    ).pipe(
      map(res => this.unwrap<Product>(res))
    );
  }

  getBySku(
    sku: string,
    deleted?: boolean,
    overrideBranchId?: string
  ) {
    return this.get<ApiResponse<Product>>(
      this.endpoints.getBySku(sku),
      {
        branchId: this.resolveBranch(overrideBranchId),
        deleted
      }
    ).pipe(
      map(res => this.unwrap<Product>(res))
    );
  }

  getByCategory(
    categoryId: number,
    deleted?: boolean,
    strict = false,
    overrideBranchId?: string
  ) {
    return this.get<ApiResponse<Product[]>>(
      this.endpoints.byCategory(categoryId),
      {
        branchId: this.resolveBranch(overrideBranchId),
        deleted,
        strict
      }
    ).pipe(
      map(res => this.unwrap<Product[]>(res) ?? [])
    );
  }

  getBySupplier(
    supplierId: string,
    deleted?: boolean,
    overrideBranchId?: string
  ) {
    return this.get<ApiResponse<Product[]>>(
      this.endpoints.bySupplier(supplierId),
      {
        branchId: this.resolveBranch(overrideBranchId),
        deleted
      }
    ).pipe(
      map(res => this.unwrap<Product[]>(res) ?? [])
    );
  }

  getAudits(
    productId: string,
    overrideBranchId?: string
  ) {
    return this.get<ApiResponse<any[]>>(
      this.endpoints.audits(productId),
      {
        branchId: this.resolveBranch(overrideBranchId)
      }
    ).pipe(
      map(res => this.unwrap<any[]>(res) ?? [])
    );
  }

  /* ================= CREATE ================= */

  create(
    payload: FormData,
    overrideBranchId?: string
  ) {
    return this.post<ApiResponse<Product>>(
      this.endpoints.bulk.fullCreate,
      payload,
      {
        branchId: this.resolveBranch(overrideBranchId)
      }
    ).pipe(
      map(res => this.unwrap<Product>(res))
    );
  }

  bulkFullCreate(
    formData: FormData,
    overrideBranchId?: string
  ) {
    return this.post<BulkResult<Product>>(
      this.endpoints.bulk.fullCreate,
      formData,
      {
        branchId: this.resolveBranch(overrideBranchId)
      }
    );
  }

  /* ================= UPDATE ================= */

  update(
    id: string,
    payload: ProductUpdateDTO,
    overrideBranchId?: string
  ) {
    return this.put<ApiResponse<Product>>(
      this.endpoints.update(id),
      payload,
      {
        branchId: this.resolveBranch(overrideBranchId)
      }
    ).pipe(
      map(res => this.unwrap<Product>(res))
    );
  }

  /* ================= DELETE ================= */

  softDelete(
    id: string,
    reason?: string
  ) {
    return this.delete<void>(
      this.endpoints.softDelete(id),
      { reason }
    );
  }

  restore(
    id: string,
    reason?: string,
    restoreOptions?: {
      restoreInventory?: boolean;
      restoreStockTransactions?: boolean;
    }
  ) {
    return this.post<void>(
      this.endpoints.restore(id),
      restoreOptions ?? {},
      { reason }
    );
  }

  hardDelete(
    id: string,
    reason?: string,
    overrideBranchId?: string
  ) {
    return this.delete<void>(
      this.endpoints.hardDelete(id),
      {
        branchId: this.resolveBranch(overrideBranchId),
        reason
      }
    );
  }

  bulkSoftDelete(
    ids: string[],
    reason?: string
  ) {
    return this.http.delete<void>(
      `${this.api}${this.endpoints.bulk.softDelete}`,
      {
        body: { ids, reason }
      }
    );
  }

  bulkRestore(
    ids: string[],
    reason?: string,
    restoreOptions?: {
      restoreInventory?: boolean;
      restoreStockTransactions?: boolean;
    }
  ) {
    return this.put<void>(
      this.endpoints.bulk.restore,
      {
        ids,
        reason,
        restoreOptions
      }
    );
  }

  bulkHardDelete(
    ids: string[],
    reason?: string,
    overrideBranchId?: string
  ) {
    return this.http.delete<void>(
      `${this.api}${this.endpoints.bulk.hardDelete}`,
      {
        body: { ids, reason },
        params: this.buildParams({
          branchId: this.resolveBranch(overrideBranchId)
        })
      }
    );
  }

  /* ================= IMAGES ================= */

  getImages(
    productId: string,
    deleted?: boolean,
    overrideBranchId?: string
  ) {
    return this.get<ProductImage[]>(
      this.endpoints.images.list(productId),
      {
        branchId: this.resolveBranch(overrideBranchId),
        deleted
      }
    );
  }

  uploadImages(
    productId: string,
    files: File[],
    overrideBranchId?: string
  ) {
    const formData = new FormData();

    files.forEach(file => formData.append('files', file));

    return this.patch<void>(
      this.endpoints.images.upload(productId),
      formData,
      {
        branchId: this.resolveBranch(overrideBranchId)
      }
    );
  }

  deleteImage(
    productId: string,
    filename: string,
    soft = true,
    reason?: string,
    overrideBranchId?: string
  ) {
    return this.delete<void>(
      this.endpoints.images.deleteOne(
        productId,
        filename
      ),
      {
        branchId:
          this.resolveBranch(
            overrideBranchId
          ),
        soft,
        reason
      }
    );
  }

  deleteAllImages(
    productId: string,
    soft = true,
    reason?: string,
    overrideBranchId?: string
  ) {
    return this.delete<void>(
      this.endpoints.images.deleteAll(productId),
      {
        branchId: this.resolveBranch(overrideBranchId),
        soft,
        reason
      }
    );
  }

  restoreImage(
    productId: string,
    imageId: string,
    reason?: string,
    overrideBranchId?: string
  ) {
    return this.put<void>(
      this.endpoints.images.restore(productId, imageId),
      {},
      {
        branchId: this.resolveBranch(overrideBranchId),
        reason
      }
    );
  }

  getImageBlob(
    productId: string,
    fileName: string,
    overrideBranchId?: string,
    version?: number
  ) {

    return this.http.get(
      `${this.api}/products/images/shared/${fileName}`,
      {
        params: this.buildParams({
          branchId:
            this.resolveBranch(
              overrideBranchId
            ),
          v: version
        }),
        responseType: 'blob'
      }
    );
  }

  downloadImagesZip(
    productId: string,
    deleted?: boolean,
    overrideBranchId?: string
  ) {
    return this.http.get(
      `${this.api}${this.endpoints.images.zip(productId)}`,
      {
        params: this.buildParams({
          branchId: this.resolveBranch(overrideBranchId),
          deleted
        }),
        responseType: 'blob'
      }
    );
  }

  getThumbnail(
    productId: string,
    overrideBranchId?: string
  ) {
    return this.http.get(
      `${this.api}${this.endpoints.thumbnail(productId)}`,
      {
        params: this.buildParams({
          branchId: this.resolveBranch(overrideBranchId)
        }),
        responseType: 'blob'
      }
    );
  }

  getThumbnailBlob(
    productId: string,
    branchId: string
  ) {

    return this.http.get(
      this.api +
      this.endpoints.thumbnail(productId),
      {
        params: {
          branchId
        },
        responseType: 'blob'
      }
    );
  }

  getImageAudits(
    productId: string,
    overrideBranchId?: string
  ) {
    return this.get<ProductImageAudit[]>(
      this.endpoints.images.auditHistory(
        productId
      ),
      {
        branchId:
          this.resolveBranch(
            overrideBranchId
          )
      }
    );
  }
}