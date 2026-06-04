import {
  Component,
  Inject,
  inject,
  OnInit
} from '@angular/core';

import {
  CommonModule
} from '@angular/common';

import {
  MAT_DIALOG_DATA,
  MatDialogModule,
  MatDialogRef
} from '@angular/material/dialog';

import {
  MatButtonModule
} from '@angular/material/button';

import {
  MatIconModule
} from '@angular/material/icon';

import {
  SessionService
} from '../../../../../core/services/session.service';

import {
  UserSessionDto
} from '../../../../../core/models/session.models';

@Component({
  selector: 'app-logout-choice-dialog',
  standalone: true,
  imports: [
    CommonModule,
    MatDialogModule,
    MatButtonModule,
    MatIconModule
  ],
  templateUrl:
    './logout-choice-dialog.component.html',
  styleUrls: [
    './logout-choice-dialog.component.scss'
  ]
})
export class LogoutChoiceDialogComponent
  implements OnInit {

  private sessionService =
    inject(SessionService);

  sessions: UserSessionDto[] = [];

  currentSession:
    UserSessionDto | null = null;

  otherSessions:
    UserSessionDto[] = [];

  loading = true;

  busy = false;

  constructor(
    private dialogRef:
      MatDialogRef<LogoutChoiceDialogComponent>,
    @Inject(MAT_DIALOG_DATA)
    public data: any
  ) {
  }

  ngOnInit() {

    this.load();

  }

  load() {

    this.loading = true;

    this.sessionService
      .getActiveSessions()
      .subscribe({

        next: sessions => {

          this.sessions = sessions;

          this.currentSession =
            sessions.find(
              x => x.current
            ) ?? null;

          this.otherSessions =
            sessions.filter(
              x => !x.current
            );

          this.loading = false;
        },

        error: () => {

          this.loading = false;

        }
      });
  }

  logoutCurrent() {

    this.dialogRef.close(
      'current'
    );

  }

  logoutAll() {

    this.dialogRef.close(
      'all'
    );

  }

  logoutOthers() {

    this.dialogRef.close(
      'others'
    );

  }
}