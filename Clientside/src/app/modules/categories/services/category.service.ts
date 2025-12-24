import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Category } from '../models/category.model';
import { environment } from '../../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class CategoryService {

  private base = environment.apiUrl + environment.endpoints.categories.base;

  constructor(private http: HttpClient) {}

  getAll(
    mode: 'tree' | 'flat' = 'tree',
    deleted = false
  ) {
    return this.http.get<Category[]>(
      `${this.base}/all`,
      {
        params: {
          mode,
          deleted: String(deleted)
        }
      }
    );
  }

  search(keyword: string, deleted = false) {
    return this.http.get<Category[]>(
      `${this.base}/search`,
      {
        params: {
          keyword,
          deleted: String(deleted)
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
}