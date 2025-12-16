import { Injectable } from "@angular/core";
import { ProductVariant } from "../models/product-variant.model";
import { environment } from '../../../../../environments/environment';
import { HttpClient } from "@angular/common/http";

@Injectable({ providedIn: 'root' })
export class ProductVariantService {

  private base = environment.apiUrl + environment.endpoints.products.variants.base;

  constructor(private http: HttpClient) {}

  forProduct(productId: string) {
    return this.http.get<ProductVariant[]>(
      `${this.base}/product/${productId}`
    );
  }

  create(dto: Partial<ProductVariant>) {
    return this.http.post<ProductVariant>(this.base, dto);
  }

  update(id: string, dto: Partial<ProductVariant>) {
    return this.http.put<ProductVariant>(`${this.base}/${id}`, dto);
  }

  delete(id: string) {
    return this.http.delete<void>(`${this.base}/${id}`);
  }
}