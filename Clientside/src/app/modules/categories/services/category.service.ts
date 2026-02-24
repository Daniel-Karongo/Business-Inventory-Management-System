import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Category } from '../models/category.model';
import { environment } from '../../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class CategoryService {

  private base = environment.apiUrl + environment.endpoints.categories.base;

  constructor(private http: HttpClient) { }

  getAll(mode: 'tree' | 'flat' = 'tree', deleted: boolean | null = false) {
    return this.http.get<Category[]>(`${this.base}/all`, {
      params: {
        mode,
        deleted: deleted === null ? '' : String(deleted)
      }
    });
  }

  search(keyword: string, deleted: boolean | null = false) {
    return this.http.get<Category[]>(
      `${this.base}/search`,
      {
        params: {
          keyword,
          deleted: deleted === null ? '' : String(deleted)
        }
      }
    );
  }

  getById(id: number, mode: 'tree' | 'flat' = 'tree', deleted = false) {
    return this.http.get<Category>(
      `${this.base}/${id}`,
      {
        params: {
          mode,
          deleted: String(deleted)
        }
      }
    );
  }

  create(payload: any) {
    return this.http.post<Category>(this.base, payload);
  }

  update(id: number, payload: any) {
    return this.http.patch<Category>(`${this.base}/${id}/recursive`, payload);
  }

  softDelete(id: number) {
    return this.http.delete(`${this.base}/${id}/soft`);
  }

  restore(id: number) {
    return this.http.put(`${this.base}/${id}/restore`, {});
  }

  bulkSoftDelete(ids: number[]) {
    return this.http.delete(`${this.base}/bulk/soft`, { body: ids });
  }

  bulkRestore(ids: number[]) {
    return this.http.put(`${this.base}/restore/bulk`, ids);
  }
}