import { Injectable, NgZone, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';

import { AuthService } from '../../modules/auth/services/auth.service';
import { environment } from '../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class IdleLogoutService implements OnDestroy {

  private timeoutId: any;

  private loggingOut = false;

  private started = false;

  private logoutSub?: Subscription;

  private readonly idleMs =
    environment.idleLogoutMinutes * 60 * 1000;

  private readonly events = [
    'mousemove',
    'mousedown',
    'keydown',
    'scroll',
    'touchstart',
    'click'
  ];

  private readonly boundResetTimer =
    this.resetTimer.bind(this);

  constructor(
    private auth: AuthService,
    private router: Router,
    private zone: NgZone
  ) { }

  /* =========================
     PUBLIC API
     ========================= */

  start(): void {

    if (this.started) {
      return;
    }

    this.started = true;

    this.loggingOut = false;

    this.zone.runOutsideAngular(() => {

      this.events.forEach(event => {

        window.addEventListener(
          event,
          this.boundResetTimer,
          true
        );

      });

    });

    this.resetTimer();
  }

  stop(): void {

    this.started = false;

    clearTimeout(this.timeoutId);

    this.events.forEach(event => {

      window.removeEventListener(
        event,
        this.boundResetTimer,
        true
      );

    });

    this.logoutSub?.unsubscribe();
  }

  ngOnDestroy(): void {
    this.stop();
  }

  /* =========================
     INTERNALS
     ========================= */

  private resetTimer(): void {

    if (!this.started) {
      return;
    }

    clearTimeout(this.timeoutId);

    this.timeoutId = setTimeout(() => {

      this.zone.run(() => {
        this.logout();
      });

    }, this.idleMs);
  }

  private logout(): void {

    if (this.loggingOut) {
      return;
    }

    this.loggingOut = true;

    this.stop();

    this.logoutSub =
      this.auth.logout().subscribe({

        next: () => {
          this.finishLogout();
        },

        error: () => {
          this.finishLogout();
        }

      });
  }

  private finishLogout(): void {

    this.auth.clearLocalState();

    this.router.navigate(
      ['/auth'],
      {
        replaceUrl: true,
        queryParams: {
          reason: 'idle'
        }
      }
    );
  }
}