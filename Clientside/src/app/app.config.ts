import { ApplicationConfig, provideZoneChangeDetection, provideAppInitializer, inject } from '@angular/core';
import { provideRouter } from '@angular/router';
import { authInterceptor } from './modules/auth/interceptors/auth.interceptor';
import { routes } from './app.routes';
import { provideHttpClient, withInterceptors } from '@angular/common/http';
import { IconLoader } from './core/utils/icon-loader';
import { provideAnimations } from '@angular/platform-browser/animations';
import { loggingInterceptor } from './core/interceptors/logging.interceptor';

export const appConfig: ApplicationConfig = {
  providers: [
    provideZoneChangeDetection({ eventCoalescing: true }),
    provideRouter(routes),
    provideHttpClient(withInterceptors([
      authInterceptor,
      loggingInterceptor
    ])),
    provideAnimations(),

    // ⭐ Correct Angular 17 initializer pattern
    provideAppInitializer(() => {
      const iconLoader = inject(IconLoader);
      iconLoader.load();
    })
  ]
};