import { HttpInterceptorFn } from '@angular/common/http';
import { inject } from '@angular/core';
import { DomainContextService } from '../services/domain-context.service';

export const tenantInterceptor: HttpInterceptorFn = (req, next) => {

  const domain = inject(DomainContextService);

  const tenant = domain.tenantCode;

  if (!tenant) {
    return next(req);
  }

  const cloned = req.clone({
    setHeaders: {
      'X-Tenant': tenant
    }
  });

  return next(cloned);
};