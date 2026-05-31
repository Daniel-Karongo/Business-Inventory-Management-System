import { Component, inject } from '@angular/core';
import { MatButtonModule } from '@angular/material/button';
import { MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatIconModule } from '@angular/material/icon';

@Component({
  standalone: true,
  selector: 'app-biometric-prompt',
  imports: [
    MatDialogModule,
    MatButtonModule,
    MatIconModule
  ],
  template: `
    <div class="biometric-dialog">

      <!-- HEADER -->
      <div class="dialog-header">

        <div class="icon-shell">
          <mat-icon>fingerprint</mat-icon>
        </div>

        <div class="header-copy">
          <h2>Enable Biometrics</h2>

          <p>
            Secure your account with fingerprint or face
            authentication on this device.
          </p>
        </div>

      </div>

      <!-- CONTENT -->
      <div class="dialog-content">

        <div class="feature-list">

          <div class="feature-item">
            <mat-icon>bolt</mat-icon>

            <span>
              Faster sign in experience
            </span>
          </div>

          <div class="feature-item">
            <mat-icon>shield</mat-icon>

            <span>
              Enterprise-grade device authentication
            </span>
          </div>

          <div class="feature-item">
            <mat-icon>devices</mat-icon>

            <span>
              Works only on this registered device
            </span>
          </div>

        </div>

        <div class="security-note">
          Your biometric data never leaves your device.
        </div>

      </div>

      <!-- ACTIONS -->
      <div class="dialog-actions">

        <button
          mat-stroked-button
          type="button"
          (click)="close(false)"
        >
          Not Now
        </button>

        <button
          mat-flat-button
          color="primary"
          type="button"
          (click)="close(true)"
        >
          <mat-icon>fingerprint</mat-icon>

          Enable Biometrics
        </button>

      </div>

    </div>
  `,
  styles: [`
    :host {
      display: block;
    }

    .biometric-dialog {
      width: 100%;
      max-width: 440px;
      color: var(--text-primary);
      color: var(--text-primary);
      background: transparent;
      border-radius: inherit;
      overflow-x: hidden;
      overflow-y: auto;
      max-height: calc(90vh - 2px);
      box-sizing: border-box;
    }

    /* ======================================================
       HEADER
    ====================================================== */

    .dialog-header {
      display: flex;
      gap: 18px;

      padding: 22px 22px 16px 22px;

      background: var(--surface);
      border-bottom: 1px solid var(--border);
    }

    body.dark .dialog-header {
      background:
        linear-gradient(
          180deg,
          rgba(59, 130, 246, 0.08),
          transparent
        );
    }

    .icon-shell {
      width: 58px;
      height: 58px;

      min-width: 58px;

      display: flex;
      align-items: center;
      justify-content: center;

      border-radius: 18px;

      background:
        linear-gradient(
          135deg,
          rgba(0, 122, 255, 0.16),
          rgba(0, 122, 255, 0.08)
        );

      backdrop-filter: blur(10px);

      border: 1px solid rgba(0, 122, 255, 0.14);

      box-shadow:
        0 10px 24px rgba(0, 122, 255, 0.14),
        inset 0 0 0 1px rgba(255,255,255,0.06);
    }

    .icon-shell mat-icon {
      font-size: 30px;
      width: 30px;
      height: 30px;

      color: #007aff;
    }

    .header-copy {
      display: flex;
      flex-direction: column;
      gap: 8px;
    }

    .header-copy h2 {
      margin: 0;

      font-size: 1.28rem;
      line-height: 1.15;

      letter-spacing: -0.4px;

      font-weight: 650;

      color: var(--text-primary);
    }

    .header-copy p {
      margin: 0;

      color: var(--text-secondary);

      font-size: 0.88rem;

      line-height: 1.6;
    }

    /* ======================================================
       CONTENT
    ====================================================== */

    .dialog-content {
      padding: 18px 22px 0 22px;
    }

    .feature-list {
      display: flex;
      flex-direction: column;
      gap: 12px;
    }

    .feature-item {
      display: flex;
      align-items: center;

      gap: 14px;

      padding: 14px 16px;

      border-radius: 12px;

      background: var(--surface);
      border: 1px solid var(--border);
      box-shadow: none;

      transition:
        background-color 160ms ease,
        transform 140ms ease,
        box-shadow 160ms ease;
    }

    .feature-item:hover {
      background: var(--surface-hover);

      transform: translateX(2px);
    }

    .feature-item mat-icon {
      color: #007aff;

      font-size: 20px;
      width: 20px;
      height: 20px;

      flex-shrink: 0;
    }

    .icon-shell mat-icon,
    .feature-item mat-icon {
      color: var(--active);
    }

    .feature-item span {
      font-size: 0.92rem;

      line-height: 1.45;

      color: var(--text-primary);

      font-weight: 500;

      letter-spacing: 0.1px;
    }

    .security-note {
      margin-top: 16px;

      padding: 12px 14px;

      border-radius: 10px;

      background: var(--surface-hover);
      border: 1px solid var(--border);

      color: var(--text-secondary);

      font-size: 0.82rem;

      line-height: 1.5;
    }

    /* ======================================================
       ACTIONS
    ====================================================== */

    .dialog-actions {
      display: flex;
      justify-content: flex-end;

      gap: 12px;

      margin-top: 18px;

      padding: 18px 22px 22px 22px;
    }

    button[mat-flat-button] {
      min-width: 185px;
    }

    button mat-icon {
      font-size: 18px;
      width: 18px;
      height: 18px;
    }

    /* ======================================================
       MOBILE
    ====================================================== */

    @media (max-width: 600px) {

      .biometric-dialog {

        max-width: 100%;

        border-radius: 16px;
      }

      .dialog-header {

        padding: 18px 18px 12px 18px;

        gap: 14px;
      }

      .dialog-content {

        padding: 14px 18px 0 18px;
      }

      .dialog-actions {

        flex-direction: column-reverse;

        padding: 14px 18px 18px 18px;

        gap: 10px;
      }

      .dialog-actions button {

        width: 100%;
      }

      button[mat-flat-button] {

        min-width: unset;
      }

      .icon-shell {

        width: 50px;
        height: 50px;
        min-width: 50px;
      }

      .header-copy h2 {

        font-size: 1.08rem;
      }

      .header-copy p {

        font-size: 0.82rem;

        line-height: 1.5;
      }

      .feature-item {

        padding: 12px 13px;

        gap: 12px;
      }

      .feature-item span {

        font-size: 0.86rem;
      }

      .security-note {

        font-size: 0.78rem;
      }
    }
  `]
})
export class BiometricPromptDialog {

  private readonly dialogRef =
    inject(MatDialogRef<BiometricPromptDialog>);

  close(val: boolean): void {
    this.dialogRef.close(val);
  }
}