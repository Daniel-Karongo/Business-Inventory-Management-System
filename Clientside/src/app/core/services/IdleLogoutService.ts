import { Injectable, NgZone, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { AuthService } from '../../modules/auth/services/auth.service';
import { environment } from '../../../environments/environment';

@Injectable({ providedIn: 'root' })
export class IdleLogoutService implements OnDestroy {

  private timeoutId: any;
  private readonly idleMs = environment.idleLogoutMinutes * 60 * 1000;

  private readonly events = [
    'mousemove',
    'mousedown',
    'keydown',
    'scroll',
    'touchstart'
  ];

  constructor(
    private auth: AuthService,
    private router: Router,
    private zone: NgZone
  ) {}

  start() {
    this.resetTimer();
    this.zone.runOutsideAngular(() => {
      this.events.forEach(e =>
        document.addEventListener(e, this.resetTimer, true)
      );
    });
  }

  stop() {
    clearTimeout(this.timeoutId);
    this.events.forEach(e =>
      document.removeEventListener(e, this.resetTimer, true)
    );
  }

  private resetTimer = () => {
    clearTimeout(this.timeoutId);
    this.timeoutId = setTimeout(() => this.logout(), this.idleMs);
  };

  private logout() {
    this.zone.run(() => {
      this.auth.logout();
      this.router.navigate(['/login'], {
        queryParams: { reason: 'idle' }
      });
    });
  }

  ngOnDestroy() {
    this.stop();
  }
}