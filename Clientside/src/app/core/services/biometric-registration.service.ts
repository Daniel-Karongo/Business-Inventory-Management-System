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

  async register(): Promise<boolean> {

    const deviceId =
      this.device.getDeviceId();

    return new Promise<boolean>((resolve) => {

      this.biometricApi
        .hasCredentialForCurrentDevice(
          deviceId
        )
        .subscribe({

          next: exists => {

            if (exists) {

              const ref =
                this.dialog.open(
                  OverwriteBiometricDialog
                );

              ref.afterClosed()
                .subscribe(choice => {

                  if (choice === 'reuse') {

                    this.snack.open(
                      'Using existing biometric',
                      'Close',
                      { duration: 3000 }
                    );

                    resolve(false);
                    return;

                  }

                  if (choice === 'overwrite') {

                    this.startRegistration(
                      deviceId,
                      resolve
                    );

                    return;
                  }

                  resolve(false);

                });

              return;
            }

            this.startRegistration(
              deviceId,
              resolve
            );

          },

          error: () => {

            this.snack.open(
              'Failed checking existing credential',
              'Close',
              { duration: 4000 }
            );

            resolve(false);

          }

        });

    });

  }

  private startRegistration(
    deviceId: string,
    resolve: (v: boolean) => void
  ) {

    this.auth.biometricRegisterStart(
      deviceId
    ).subscribe({

      next: async options => {

        try {

          const credential =
            await this.webauthn.register(
              options
            );

          this.auth.biometricRegisterFinish(
            credential,
            deviceId
          ).subscribe({

            next: () => {

              this.snack.open(
                'Biometric enabled successfully',
                'Close',
                { duration: 3000 }
              );

              resolve(true);

            },

            error: () => {

              this.snack.open(
                'Biometric setup failed',
                'Close',
                { duration: 4000 }
              );

              resolve(false);

            }

          });

        }
        catch {

          resolve(false);

        }

      },

      error: () => {

        resolve(false);

      }

    });

  }
}