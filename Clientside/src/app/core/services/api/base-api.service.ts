import { HttpClient, HttpParams } from '@angular/common/http';
import { inject, Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { environment } from '../../../../environments/environment';
import { ApiResponse } from '../../models/api-response.model';

@Injectable({
  providedIn: 'root'
})
export class BaseApiService {
  protected http = inject(HttpClient);

  protected api = environment.apiUrl;

  protected buildParams(
    query?: Record<string, any>
  ): HttpParams {
    let params = new HttpParams();

    if (!query) {
      return params;
    }

    Object.entries(query)
      .forEach(([key, value]) => {
        if (
          value !== null &&
          value !== undefined &&
          value !== ''
        ) {
          params = params.set(key, value);
        }
      });

    return params;
  }

  protected get<T>(
    url: string,
    query?: Record<string, any>
  ): Observable<T> {
    return this.http.get<T>(
      `${this.api}${url}`,
      {
        params: this.buildParams(query)
      }
    );
  }

  protected post<T>(
    url: string,
    body?: unknown,
    query?: Record<string, any>
  ): Observable<T> {
    return this.http.post<T>(
      `${this.api}${url}`,
      body,
      {
        params: this.buildParams(query)
      }
    );
  }

  protected put<T>(
    url: string,
    body?: unknown,
    query?: Record<string, any>
  ): Observable<T> {
    return this.http.put<T>(
      `${this.api}${url}`,
      body,
      {
        params: this.buildParams(query)
      }
    );
  }

  protected patch<T>(
    url: string,
    body?: unknown,
    query?: Record<string, any>
  ): Observable<T> {
    return this.http.patch<T>(
      `${this.api}${url}`,
      body,
      {
        params: this.buildParams(query)
      }
    );
  }

  protected delete<T>(
    url: string,
    query?: Record<string, any>
  ): Observable<T> {
    return this.http.delete<T>(
      `${this.api}${url}`,
      {
        params: this.buildParams(query)
      }
    );
  }

  protected unwrap<T>(
    response: ApiResponse<T>
  ): T {
    return response.data as T;
  }
}