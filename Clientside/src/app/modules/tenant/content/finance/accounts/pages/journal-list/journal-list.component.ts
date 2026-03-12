import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';

import { MatTableModule } from '@angular/material/table';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import { finalize } from 'rxjs/operators';

import {
  JournalService,
  JournalEntry
} from '../../services/journal.service';
import { MatTooltipModule } from '@angular/material/tooltip';

@Component({
  selector: 'app-journal-list',
  standalone: true,
  imports: [
    CommonModule,
    MatTableModule,
    MatIconModule,
    MatButtonModule,
    MatSnackBarModule,
    MatTooltipModule
  ],
  templateUrl: './journal-list.component.html',
  styleUrls: ['./journal-list.component.scss']
})
export class JournalListComponent implements OnInit {

  displayedColumns = [
    'reference',
    'description',
    'source',
    'postedAt',
    'status',
    'actions'
  ];

  journals: JournalEntry[] = [];
  loading = false;

  constructor(
    private journalService: JournalService,
    private router: Router,
    private snackbar: MatSnackBar
  ) {}

  ngOnInit(): void {
    this.load();
  }

  load() {
    this.loading = true;

    this.journalService.list()
      .pipe(finalize(() => (this.loading = false)))
      .subscribe({
        next: data => {
          this.journals = data || [];
        },
        error: () => {
          this.snackbar.open(
            'Failed to load journals',
            'Close',
            { duration: 3000 }
          );
        }
      });
  }

  view(j: JournalEntry) {
    this.router.navigate(['/accounts/journals', j.id]);
  }
}