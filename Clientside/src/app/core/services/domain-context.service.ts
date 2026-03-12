import { Injectable } from '@angular/core';

export interface DomainContext {
  host: string;
  subdomain: string | null;
  isPlatform: boolean;
  isTenant: boolean;
  tenantCode: string | null;
}

@Injectable({ providedIn: 'root' })
export class DomainContextService {

  private cached?: DomainContext;

  getContext(): DomainContext {

    if (this.cached) {
      return this.cached;
    }

    const host = window.location.hostname;
    const parts = host.split('.');

    let subdomain: string | null = null;

    if (parts.length > 1) {
      subdomain = parts[0];
    }

    const isPlatform = subdomain === 'platform';

    const isTenant =
      subdomain !== null &&
      subdomain !== 'platform' &&
      subdomain !== 'www';

    const tenantCode = isTenant ? subdomain : null;

    this.cached = {
      host,
      subdomain,
      isPlatform,
      isTenant,
      tenantCode
    };

    return this.cached;
  }

  get tenantCode(): string | null {
    return this.getContext().tenantCode;
  }

  get isPlatform(): boolean {
    return this.getContext().isPlatform;
  }

  get isTenant(): boolean {
    return this.getContext().isTenant;
  }
}