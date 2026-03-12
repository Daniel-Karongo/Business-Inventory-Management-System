import { Component, Inject } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';

@Component({
  selector: 'app-logout-choice-dialog',
  standalone: true,
  templateUrl: './logout-choice-dialog.component.html',
  styleUrls: ['./logout-choice-dialog.component.scss']
})
export class LogoutChoiceDialogComponent {

  count = 1;

  constructor(
    private dialogRef: MatDialogRef<LogoutChoiceDialogComponent>,
    @Inject(MAT_DIALOG_DATA) data: { count: number } | null
  ) {
    this.count = data?.count ?? 1;
  }

  logoutCurrent() {
    this.dialogRef.close('current');
  }

  logoutAll() {
    this.dialogRef.close('all');
  }
}