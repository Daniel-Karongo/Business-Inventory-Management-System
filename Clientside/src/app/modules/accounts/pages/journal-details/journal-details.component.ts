import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ActivatedRoute, Router } from '@angular/router';
import { HttpClient } from '@angular/common/http';

import { MatButtonModule } from '@angular/material/button';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import { ReasonDialogComponent } from '../../../../shared/components/reason-dialog/reason-dialog.component';
import { environment } from '../../../../../environments/environment';

@Component({
  standalone: true,
  imports: [
    CommonModule,
    MatButtonModule,
    MatDialogModule,
    MatSnackBarModule
  ],
  templateUrl: './journal-details.component.html',
  styleUrls: ['./journal-details.component.scss']
})
export class JournalDetailsComponent {

  journal: any;
  loading = true;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private http: HttpClient,
    private dialog: MatDialog,
    private snackbar: MatSnackBar
  ) {
    const id = this.route.snapshot.paramMap.get('id');
    if (!id) return;

    this.http.get(`${environment.apiUrl}/accounting/journals/${id}`)
      .subscribe({
        next: j => {
          this.journal = j;
          this.loading = false;
        },
        error: () => {
          this.snackbar.open('Failed to load journal', 'Close', { duration: 3000 });
          this.loading = false;
        }
      });
  }

  /* ============================================================
     REVERSAL
  ============================================================ */

  reverse() {
    const ref = this.dialog.open(ReasonDialogComponent, {
      width: '420px',
      data: {
        title: 'Reverse Journal?',
        message: 'Provide a reason for reversing this journal. This action is permanent.',
        action: 'REVERSE',
        confirmText: 'Reverse Journal'
      }
    });

    ref.afterClosed().subscribe(result => {
      if (!result?.confirmed) return;

      this.http.post(
        `${environment.apiUrl}/accounting/journals/${this.journal.id}/reverse`,
        { reason: result.reason }
      ).subscribe({
        next: () => {
          this.snackbar.open('Journal reversed successfully', 'Close', { duration: 2500 });
          this.journal.reversed = true;
        },
        error: err => {
          this.snackbar.open(
            err?.error?.message || 'Failed to reverse journal',
            'Close',
            { duration: 4000 }
          );
        }
      });
    });
  }

  back() {
    this.router.navigate(['/accounts/journals']);
  }
}