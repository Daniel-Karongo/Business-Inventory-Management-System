import { Component, inject } from '@angular/core';
import { MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';

@Component({
  standalone: true,
  selector: 'app-overwrite-biometric-dialog',
  template: `
    <h2 mat-dialog-title>Biometric Exists</h2>

    <mat-dialog-content>
      A biometric is already registered on this device.
    </mat-dialog-content>

    <mat-dialog-actions align="end">
      <button mat-button (click)="close('reuse')">Use Existing</button>
      <button mat-flat-button color="primary" (click)="close('overwrite')">
        Overwrite
      </button>
    </mat-dialog-actions>
  `,
  imports: [MatDialogModule, MatButtonModule]
})
export class OverwriteBiometricDialog {

  private ref = inject(MatDialogRef<OverwriteBiometricDialog>);

  close(val: 'reuse' | 'overwrite') {
    this.ref.close(val);
  }
}