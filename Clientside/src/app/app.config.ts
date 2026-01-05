import {
  ApplicationConfig,
  provideZoneChangeDetection,
  provideAppInitializer,
  inject
} from '@angular/core';
import { provideRouter } from '@angular/router';
import { provideHttpClient, withInterceptors } from '@angular/common/http';
import { provideAnimations } from '@angular/platform-browser/animations';
import { provideNativeDateAdapter } from '@angular/material/core';
import { firstValueFrom } from 'rxjs';

import { routes } from './app.routes';
import { authInterceptor } from './modules/auth/interceptors/auth.interceptor';
import { loggingInterceptor } from './core/interceptors/logging.interceptor';

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
    provideNativeDateAdapter(),

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
    })
  ]
};