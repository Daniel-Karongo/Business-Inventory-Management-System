import { Injectable, NgZone, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { AuthService } from '../../modules/auth/services/auth.service';
import { environment } from '../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class IdleLogoutService implements OnDestroy {

  private timeoutId: any;
  private loggingOut = false;

  private readonly idleMs =
    environment.idleLogoutMinutes * 60 * 1000;

  private readonly events = [
    'mousemove',
    'mousedown',
    'keydown',
    'scroll',
    'touchstart'
  ];

  private readonly boundResetTimer = () => this.resetTimer();

  constructor(
    private auth: AuthService,
    private router: Router,
    private zone: NgZone
  ) {
  }

  /* =========================
     PUBLIC API
     ========================= */

  start(): void {
    this.resetTimer();

    this.zone.runOutsideAngular(() => {
      this.events.forEach(event =>
        document.addEventListener(event, this.boundResetTimer, true)
      );
    });
  }

  stop(): void {
    clearTimeout(this.timeoutId);

    this.events.forEach(event =>
      document.removeEventListener(event, this.boundResetTimer, true)
    );
  }

  ngOnDestroy(): void {
    this.stop();
  }

  /* =========================
     INTERNALS
     ========================= */

  private resetTimer(): void {
    clearTimeout(this.timeoutId);

    this.timeoutId = setTimeout(() => {
      this.logout();
    }, this.idleMs);
  }

  private logout(): void {
    if (this.loggingOut) return;

    this.loggingOut = true;
    this.stop();

    this.zone.run(() => {
      this.auth.logout().subscribe({
        next: () => this.finishLogout(),
        error: () => this.finishLogout()
      });
    });
  }

  private finishLogout(): void {
    this.auth.clearLocalState();
    this.loggingOut = false;

    this.router.navigate(['/login'], {
      replaceUrl: true,
      queryParams: { reason: 'idle' }
    });
  }
}