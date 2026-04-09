import { Injectable, inject } from '@angular/core';
import { AuthService, LoginRequest } from '../../modules/auth/services/auth.service';
import { DeviceService } from './device.service';
import { LocationService } from './location.service';
import { WebAuthnService } from './webauthn.service';
import { MatSnackBar } from '@angular/material/snack-bar';
import { BiometricApiService } from './biometric-api.service';
import { MatDialog } from '@angular/material/dialog';
import { OverwriteBiometricDialog } from '../../shared/components/overwrite-biometric-dialog/overwrite-biometric-dialog.component';

@Injectable({ providedIn: 'root' })
export class BiometricRegistrationService {

  private auth = inject(AuthService);
  private device = inject(DeviceService);
  private location = inject(LocationService);
  private webauthn = inject(WebAuthnService);
  private snack = inject(MatSnackBar);
  private biometricApi = inject(BiometricApiService);
  private dialog = inject(MatDialog);

  private PROMPT_KEY_PREFIX = 'biometric_prompt_shown_';

  shouldPrompt(deviceId: string): boolean {
    return !localStorage.getItem(this.key(deviceId));
  }

  markPromptShown(deviceId: string) {
    localStorage.setItem(this.key(deviceId), '1');
  }

  private key(deviceId: string) {
    return `${this.PROMPT_KEY_PREFIX}${deviceId}`;
  }

  async register(loginPayload: LoginRequest) {

    let location;

    try {
      location = await this.location.getLocation();
    } catch {
      this.snack.open('Location required for biometric setup', 'Close', { duration: 4000 });
      return;
    }

    const deviceId = this.device.getDeviceId();

    const payload = {
      ...loginPayload,
      deviceId,
      latitude: location.latitude,
      longitude: location.longitude,
      accuracy: location.accuracy
    };

    this.auth.biometricRegisterStart(payload).subscribe({
      next: async (options) => {

        try {
          const credential = await this.webauthn.register(options);

          this.auth.biometricRegisterFinish(
            JSON.stringify(credential),
            deviceId
          ).subscribe({
            next: () => {
              this.snack.open('Biometric enabled successfully', 'Close', { duration: 3000 });
            },
            error: (err) => this.handleError(err, payload, deviceId)
          });

        } catch {
          this.snack.open('Biometric registration cancelled', 'Close', { duration: 3000 });
        }

      },
      error: (err) => this.handleError(err, payload, deviceId)
    });
  }

  private handleError(err: any, payload: LoginRequest, deviceId: string) {

    const msg = err?.error?.message ?? '';

    if (msg === 'Credential already registered') {
      this.promptOverwrite(payload, deviceId);
      return;
    }

    this.snack.open(msg || 'Biometric setup failed', 'Close', { duration: 4000 });
  }

  private promptOverwrite(payload: LoginRequest, deviceId: string) {

    const ref = this.dialog.open(OverwriteBiometricDialog);

    ref.afterClosed().subscribe(choice => {

      if (choice === 'reuse') {
        this.snack.open('Using existing biometric', 'Close', { duration: 3000 });
        return;
      }

      if (choice === 'overwrite') {
        this.deleteAndReRegister(payload, deviceId);
      }

    });
  }

  private deleteAndReRegister(payload: LoginRequest, deviceId: string) {

    this.biometricApi.list().subscribe({
      next: (list) => {

        const matches = list;

        if (!matches.length) {
          this.register(payload);
          return;
        }

        let completed = 0;

        matches.forEach(b => {
          this.biometricApi.delete(b.id).subscribe({
            next: () => {
              completed++;
              if (completed === matches.length) {
                this.register(payload);
              }
            },
            error: () => {
              this.snack.open('Failed to remove existing biometric', 'Close', { duration: 4000 });
            }
          });
        });

      },
      error: () => {
        this.snack.open('Failed to fetch biometrics', 'Close', { duration: 4000 });
      }
    });
  }
}