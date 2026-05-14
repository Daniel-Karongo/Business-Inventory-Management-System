import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';

import { MatCardModule } from '@angular/material/card';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';

@Component({
  selector: 'app-accounts-dashboard',
  standalone: true,
  imports: [
    CommonModule,
    MatCardModule,
    MatButtonModule,
    MatIconModule
  ],
  templateUrl: './accounts-dashboard.component.html',
  styleUrls: ['./accounts-dashboard.component.scss']
})
export class AccountsDashboardComponent {

  constructor(private router: Router) {}

  goChart() {
    this.router.navigate(['/app/finance/accounting/chart']);
  }

  goNewJournal() {
    this.router.navigate(['/app/finance/accounting/journals/new']);
  }

  goJournals() {
    this.router.navigate(['/app/finance/accounting/journals']);
  }

  goLedger() {
    this.router.navigate(['/app/finance/accounting/ledger']);
  }
}
