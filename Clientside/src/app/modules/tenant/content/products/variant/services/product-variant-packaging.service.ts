import { Injectable } from '@angular/core';
import { environment } from '../../../../../../../environments/environment';
import { BaseApiService } from '../../../../../../core/services/api/base-api.service';
import {
  PackagingDTO,
  CreatePackagingRequest,
  UpdatePackagingRequest
} from '../../../stock/models/packaging.model';

@Injectable({ providedIn: 'root' })
export class ProductVariantPackagingService extends BaseApiService {

  private endpoints = environment.endpoints.products.variants.packaging;

  getForVariant(variantId: string) {
    return super.get<PackagingDTO[]>(
      this.endpoints.get(variantId)
    );
  }

  getBasePackaging(variantId: string) {
    return super.get<PackagingDTO>(
      this.endpoints.basePackaging(variantId)
    );
  }

  create(payload: CreatePackagingRequest) {
    return super.post<PackagingDTO>(
      this.endpoints.create,
      payload
    );
  }

  update(id: string, payload: UpdatePackagingRequest) {
    return super.put<PackagingDTO>(
      this.endpoints.update(id),
      payload
    );
  }

  remove(id: string, branchId: string) {
    return super.delete<void>(
      this.endpoints.delete(id),
      { branchId }
    );
  }
}