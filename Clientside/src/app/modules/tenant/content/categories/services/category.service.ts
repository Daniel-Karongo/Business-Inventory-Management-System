import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { environment } from '../../../../../../environments/environment';
import { BranchContextService } from '../../../../../core/services/branch-context.service';
import { Category } from '../models/category.model';

@Injectable({ providedIn: 'root' })
export class CategoryService {

  private base = environment.apiUrl + environment.endpoints.categories.base;

  constructor(
    private http: HttpClient,
    private branchContext: BranchContextService
  ) { }

  private resolveBranch(override?: string): string {
    const branchId = override ?? this.branchContext.currentBranch;

    if (!branchId) {
      throw new Error('Branch not selected');
    }

    return branchId;
  }

  /* ================= GET ================= */

  getAll(
    mode: 'tree' | 'flat' = 'tree',
    deleted: boolean | null = false,
    overrideBranchId?: string
  ) {
    const branchId = this.resolveBranch(overrideBranchId);

    const params: any = { mode, branchId };

    if (deleted !== null) {
      params.deleted = String(deleted);
    }

    return this.http.get<Category[]>(`${this.base}/all`, { params });
  }

  search(
    keyword: string,
    deleted: boolean | null = false,
    overrideBranchId?: string
  ) {
    const branchId = this.resolveBranch(overrideBranchId);

    const params: any = { keyword, branchId };

    if (deleted !== null) {
      params.deleted = String(deleted);
    }

    return this.http.get<Category[]>(`${this.base}/search`, { params });
  }

  getById(
    id: number,
    mode: 'tree' | 'flat' = 'tree',
    deleted: boolean | null,
    overrideBranchId?: string
  ) {
    const branchId = this.resolveBranch(overrideBranchId);

    const params: any = {
      branchId,
      mode
    };

    if (deleted !== null) {
      params.deleted = String(deleted);
    }

    return this.http.get<Category>(
      `${this.base}/${id}`,
      { params }
    );
  }

  getAncestors(id: number, overrideBranchId?: string) {
    const branchId = this.resolveBranch(overrideBranchId);

    return this.http.get<Category[]>(
      `${this.base}/${id}/ancestors`,
      { params: { branchId } }
    );
  }

  /* ================= CREATE ================= */

  create(payload: any, overrideBranchId?: string) {
    const branchId = this.resolveBranch(overrideBranchId);

    return this.http.post<Category>(this.base, {
      ...payload,
      branchId
    });
  }

  /* ================= UPDATE ================= */

  update(id: number, payload: any, overrideBranchId?: string) {
    const branchId = this.resolveBranch(overrideBranchId);

    return this.http.patch<Category>(
      `${this.base}/${id}/recursive`,
      {
        ...payload,
        branchId
      }
    );
  }

  /* ================= DELETE ================= */

  softDelete(id: number, overrideBranchId?: string) {
    return this.http.delete(`${this.base}/${id}/soft`, {
      params: { branchId: this.resolveBranch(overrideBranchId) }
    });
  }

  restore(id: number, overrideBranchId?: string) {
    return this.http.put(`${this.base}/${id}/restore`, {}, {
      params: { branchId: this.resolveBranch(overrideBranchId) }
    });
  }

  restoreRecursive(id: number, overrideBranchId?: string) {
    return this.http.put(
      `${this.base}/${id}/restore-recursive`,
      {},
      { params: { branchId: this.resolveBranch(overrideBranchId) } }
    );
  }

  hardDelete(id: number, overrideBranchId?: string) {
    return this.http.delete(
      `${this.base}/${id}/hard`,
      { params: { branchId: this.resolveBranch(overrideBranchId) } }
    );
  }

  bulkHardDelete(ids: number[], overrideBranchId?: string) {
    return this.http.delete(
      `${this.base}/bulk/hard`,
      {
        body: ids,
        params: { branchId: this.resolveBranch(overrideBranchId) }
      }
    );
  }

  bulkSoftDelete(ids: number[], overrideBranchId?: string) {
    return this.http.delete(`${this.base}/bulk/soft`, {
      body: ids,
      params: { branchId: this.resolveBranch(overrideBranchId) }
    });
  }

  bulkRestore(ids: number[], overrideBranchId?: string) {
    return this.http.put(`${this.base}/restore/bulk`, ids, {
      params: { branchId: this.resolveBranch(overrideBranchId) }
    });
  }

  restoreRecursiveBulk(ids: number[], overrideBranchId?: string) {
    return this.http.put(
      `${this.base}/restore-recursive/bulk`,
      ids,
      { params: { branchId: this.resolveBranch(overrideBranchId) } }
    );
  }

  bulkImport(request: any) {
    return this.http.post<any>(
      `${this.base}/bulk-import`,
      request
    );
  }
}