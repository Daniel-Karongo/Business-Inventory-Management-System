import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { map } from 'rxjs/operators';
import { environment } from '../../../../../environments/environment';

interface PlatformUser {
  id: string;
  username: string;
}

@Injectable({ providedIn: 'root' })
export class PlatformUserApiService {

  private http = inject(HttpClient);
  private base = `${environment.apiUrl}/platform/users`;

  listAll() {
    return this.http.get<any>(this.base).pipe(
      map(res => res.content ?? [])
    );
  }
}