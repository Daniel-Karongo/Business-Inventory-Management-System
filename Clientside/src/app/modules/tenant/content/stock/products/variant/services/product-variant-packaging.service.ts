import { inject, Injectable } from '@angular/core';
import { environment } from '../../../../../../../../environments/environment';
import { BaseApiService } from '../../../../../../../core/services/api/base-api.service';
import {
  PackagingDTO,
  CreatePackagingRequest,
  UpdatePackagingRequest
} from '../../../models/packaging.model';
import { BranchContextService } from '../../../../../../../core/services/branch-context.service';

@Injectable({ providedIn: 'root' })
export class ProductVariantPackagingService extends BaseApiService {

  private endpoints = environment.endpoints.products.variants.packaging;
  private branchContext = inject(BranchContextService);

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

  create(
    payload: CreatePackagingRequest,
    overrideBranchId?: string
  ) {

    return super.post<PackagingDTO>(
      this.endpoints.create,
      {
        ...payload,
        branchId:
          this.resolveBranch(
            overrideBranchId ??
            payload.branchId
          )
      }
    );
  }

  update(
    id: string,
    payload: UpdatePackagingRequest,
    overrideBranchId?: string
  ) {

    return super.put<PackagingDTO>(
      this.endpoints.update(id),
      {
        ...payload,
        branchId:
          this.resolveBranch(
            overrideBranchId ??
            payload.branchId
          )
      }
    );
  }
  
  remove(id: string, branchId: string) {
    return super.delete<void>(
      this.endpoints.delete(id),
      { branchId }
    );
  }
}