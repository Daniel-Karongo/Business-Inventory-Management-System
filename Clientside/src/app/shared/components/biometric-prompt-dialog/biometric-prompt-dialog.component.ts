import { Component, inject } from "@angular/core";
import { MatButtonModule } from "@angular/material/button";
import { MatDialogModule, MatDialogRef } from "@angular/material/dialog";

@Component({
  standalone: true,
  selector: 'app-biometric-prompt',
  template: `
    <h2 mat-dialog-title>Enable Biometrics</h2>

    <mat-dialog-content>
      Enable biometric login on this device?
    </mat-dialog-content>

    <mat-dialog-actions align="end">
      <button mat-button (click)="close(false)">Skip</button>
      <button mat-flat-button color="primary" (click)="close(true)">
        Enable
      </button>
    </mat-dialog-actions>
  `,
  imports: [MatButtonModule, MatDialogModule]
})
export class BiometricPromptDialog {

  private dialogRef = inject(MatDialogRef<BiometricPromptDialog>);

  close(val: boolean) {
    this.dialogRef.close(val);
  }
}