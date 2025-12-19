import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Category } from '../models/category.model';
import { environment } from '../../../../environments/environment';

@Injectable({
  providedIn: 'root'
})
export class CategoryService {

  private base = environment.apiUrl + environment.endpoints.categories.base;

  constructor(
    private http: HttpClient
  ) { }

  getAll(mode: string, deleted: boolean) {
    return this.http.get<Category[]>(
      `${this.base}/all?mode=${mode}&deleted=${deleted}`
    );
  }
}
