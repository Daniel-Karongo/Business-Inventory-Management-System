import { provideHttpClient, withInterceptors } from '@angular/common/http';
import {
  ApplicationConfig,
  inject, isDevMode,
  provideAppInitializer,
  provideZoneChangeDetection
} from '@angular/core';
import {
  MAT_DATE_FORMATS,
  MAT_DATE_LOCALE,
  provideNativeDateAdapter
} from '@angular/material/core';
import { provideAnimations } from '@angular/platform-browser/animations';
import { provideRouter } from '@angular/router';

import { routes } from './app.routes';
import { loggingInterceptor } from './core/interceptors/logging.interceptor';
import { authInterceptor } from './modules/auth/interceptors/auth.interceptor';

import { DatePipe } from '@angular/common';
import { provideServiceWorker } from '@angular/service-worker';
import { APP_DATE_FORMATS } from './core/services/date-formats';
import { IconLoader } from './core/utils/icon-loader';
import { AuthService } from './modules/auth/services/auth.service';

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

    provideAppInitializer(() => {
      const auth = inject(AuthService);
      auth.loadMe().subscribe({
        error: () => auth.clearLocalState()
      });
    }), provideServiceWorker('ngsw-worker.js', {
      enabled: !isDevMode(),
      registrationStrategy: 'registerWhenStable:30000'
    }),

    DatePipe
  ]
};