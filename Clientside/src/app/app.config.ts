import {
  ApplicationConfig,
  provideZoneChangeDetection,
  provideAppInitializer,
  inject, isDevMode
} from '@angular/core';
import { provideRouter } from '@angular/router';
import { provideHttpClient, withInterceptors } from '@angular/common/http';
import { provideAnimations } from '@angular/platform-browser/animations';
import {
  provideNativeDateAdapter,
  MAT_DATE_LOCALE,
  MAT_DATE_FORMATS
} from '@angular/material/core';
import { firstValueFrom } from 'rxjs';

import { routes } from './app.routes';
import { authInterceptor } from './modules/auth/interceptors/auth.interceptor';
import { loggingInterceptor } from './core/interceptors/logging.interceptor';

import { IconLoader } from './core/utils/icon-loader';
import { AuthService } from './modules/auth/services/auth.service';
import { APP_DATE_FORMATS } from './core/services/date-formats';
import { provideServiceWorker } from '@angular/service-worker';

export const appConfig: ApplicationConfig = {
  providers: [
    provideZoneChangeDetection({ eventCoalescing: true }),

    provideRouter(routes),

    provideHttpClient(
      withInterceptors([
        authInterceptor,
        loggingInterceptor
      ])
    ),

    provideAnimations(),

    // ✅ GLOBAL DATE CONFIG
    provideNativeDateAdapter(),
    { provide: MAT_DATE_LOCALE, useValue: 'en-GB' },
    { provide: MAT_DATE_FORMATS, useValue: APP_DATE_FORMATS },

    // Load icons before app boot
    provideAppInitializer(() => {
      const iconLoader = inject(IconLoader);
      iconLoader.load();
    }),

    // Bootstrap auth state from server (/api/auth/me)
    provideAppInitializer(() => {
      const auth = inject(AuthService);

      return firstValueFrom(auth.loadMe())
        .catch(() => auth.clearLocalState());
    }), provideServiceWorker('ngsw-worker.js', {
            enabled: !isDevMode(),
            registrationStrategy: 'registerWhenStable:30000'
          })
  ]
};