import { ApplicationConfig, provideZoneChangeDetection, provideAppInitializer, inject } from '@angular/core';
import { provideRouter } from '@angular/router';
import { provideHttpClient, withInterceptors } from '@angular/common/http';
import { provideAnimations } from '@angular/platform-browser/animations';

import { routes } from './app.routes';
import { authInterceptor } from './modules/auth/interceptors/auth.interceptor';
import { loggingInterceptor } from './core/interceptors/logging.interceptor';

import { IconLoader } from './core/utils/icon-loader';
import { AuthService } from './modules/auth/services/auth.service';
import { initAuth } from './modules/auth/services/auth.service';
import { provideNativeDateAdapter } from '@angular/material/core';

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
    provideNativeDateAdapter(),

    // ⭐ Load icons before app boots
    provideAppInitializer(() => {
      const iconLoader = inject(IconLoader);
      iconLoader.load();
    }),

    // ⭐ Auto-check token expiry BEFORE app loads
    provideAppInitializer(() => {
      const auth = inject(AuthService);
      initAuth(auth);
    })
  ]
};