import { Component, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import {
  MatDialogModule,
  MatDialogRef
} from '@angular/material/dialog';

import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';

@Component({
  standalone: true,
  selector: 'app-overwrite-biometric-dialog',

  imports: [
    CommonModule,
    MatDialogModule,
    MatButtonModule,
    MatIconModule
  ],

  template: `

<div class="overwrite-shell">

  <div class="icon-wrap">
      <mat-icon>
        fingerprint
      </mat-icon>
  </div>

  <h2 mat-dialog-title>
      Passkey Already Registered
  </h2>

  <mat-dialog-content>

      <p class="primary-text">
          This device already has a passkey registered
          for your account.
      </p>

      <p class="secondary-text">
          You can continue using the existing passkey,
          or replace only the passkey stored for
          this device.
      </p>

      <div class="risk-box">
         Replacing will remove the current passkey for
         this device only. Other devices are unaffected.
      </div>

  </mat-dialog-content>

  <mat-dialog-actions align="end">

      <button
         mat-stroked-button
         (click)="close('reuse')">

         Use Existing

      </button>

      <button
         mat-flat-button
         color="primary"
         (click)="close('overwrite')">

         Replace Passkey

      </button>

  </mat-dialog-actions>

</div>

`,

  styles: [`

:host{
 display:block;
 max-width:520px;
 width:min(92vw,520px);
}

.overwrite-shell{
 padding:28px 26px 24px;
}

.icon-wrap{
 width:68px;
 height:68px;
 border-radius:50%;
 margin:0 auto 20px;
 display:flex;
 align-items:center;
 justify-content:center;
 background:rgba(37,99,235,.08);
 border:1px solid rgba(37,99,235,.15);
}

.icon-wrap mat-icon{
 font-size:34px;
 width:34px;
 height:34px;
}

h2{
 text-align:center;
 margin-bottom:18px;
}

.primary-text{
 text-align:center;
 font-weight:500;
 margin-bottom:14px;
}

.secondary-text{
 text-align:center;
 opacity:.82;
 line-height:1.55;
 margin-bottom:20px;
}

.risk-box{
 padding:14px 16px;
 border-radius:12px;
 background:var(--surface-hover);
 border:1px solid var(--surface-border);
 font-size:.88rem;
 line-height:1.5;
}

mat-dialog-actions{
 margin-top:24px;
 gap:12px;
 flex-wrap:wrap;
}

mat-dialog-actions button{
 min-width:150px;
}

@media(max-width:640px){

 .overwrite-shell{
   padding:20px 16px;
 }

 mat-dialog-actions{
   flex-direction:column;
 }

 mat-dialog-actions button{
   width:100%;
 }

}

`]

})
export class OverwriteBiometricDialog {

  private ref =
    inject(MatDialogRef<OverwriteBiometricDialog>);

  close(
    val: 'reuse' | 'overwrite'
  ) {
    this.ref.close(val);
  }

}