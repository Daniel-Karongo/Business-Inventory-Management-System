import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { BehaviorSubject, of } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { environment } from '../../../environments/environment';
import { DomainContextService } from './domain-context.service';

@Injectable({ providedIn: 'root' })
export class TenantBrandingService {

  private http = inject(HttpClient);
  private domain = inject(DomainContextService);

  private logoSubject = new BehaviorSubject<string | null>(null);

  logo$ = this.logoSubject.asObservable();

  loadLogo() {

    if (this.domain.isPlatform) {
      this.logoSubject.next(null); // fallback BM
      return;
    }

    this.http.get(
      `${environment.apiUrl}/platform/settings/logo`,
      { responseType: 'blob' }
    )
      .pipe(
        catchError(() => of(null))
      )
      .subscribe(blob => {

        if (!blob) {
          this.logoSubject.next(null); // fallback BM
          return;
        }

        const url = URL.createObjectURL(blob);
        this.logoSubject.next(url);
      });

  }

  getSnapshot(): string | null {
    return this.logoSubject.value;
  }

}