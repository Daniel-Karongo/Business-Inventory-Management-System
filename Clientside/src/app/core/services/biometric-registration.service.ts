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

  async register() {

    const deviceId = this.device.getDeviceId();

    console.log("DEVICE ID (REGISTER):", deviceId);
    
    this.auth.biometricRegisterStart(deviceId).subscribe({
      next: async (options) => {

        try {
          const credential = await this.webauthn.register(options);

          this.auth.biometricRegisterFinish(
            credential, // ✅ NOT stringified
            deviceId
          ).subscribe({
            next: () => {
              this.snack.open('Biometric enabled successfully', 'Close', { duration: 3000 });
            },
            error: (err) => this.handleError(err, deviceId)
          });

        } catch (e: any) {

          if (e.name === 'NotAllowedError') {
            this.snack.open('Biometric registration cancelled', 'Close', { duration: 3000 });
          } else {
            this.snack.open('Biometric setup failed', 'Close', { duration: 3000 });
          }
        }

      },
      error: (err) => this.handleError(err, deviceId)
    });
  }

  private handleError(err: any, deviceId: string) {

    const msg = err?.error?.message ?? '';

    if (msg === 'Credential already registered') {
      this.promptOverwrite(deviceId);
      return;
    }

    this.snack.open(msg || 'Biometric setup failed', 'Close', { duration: 4000 });
  }

  private promptOverwrite(deviceId: string) {

    const ref = this.dialog.open(OverwriteBiometricDialog);

    ref.afterClosed().subscribe(choice => {

      if (choice === 'reuse') {
        this.snack.open('Using existing biometric', 'Close', { duration: 3000 });
        return;
      }

      if (choice === 'overwrite') {
        this.deleteAndReRegister(deviceId);
      }

    });
  }

  private deleteAndReRegister(deviceId: string) {

    this.biometricApi.list().subscribe({
      next: (list) => {

        const matches = list;

        if (!matches.length) {
          this.register();
          return;
        }

        let completed = 0;

        matches.forEach(b => {
          this.biometricApi.delete(b.id).subscribe({
            next: () => {
              completed++;
              if (completed === matches.length) {
                this.register();
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