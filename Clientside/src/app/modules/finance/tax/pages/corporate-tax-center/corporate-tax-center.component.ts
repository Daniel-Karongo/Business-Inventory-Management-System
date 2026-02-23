import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';

import { MatCardModule } from '@angular/material/card';
import { MatButtonModule } from '@angular/material/button';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';

import { TaxService } from '../../services/tax.service';
import { AccountsService } from '../../../../accounts/services/accounts.service';

@Component({
  standalone: true,
  selector: 'app-corporate-tax-center',
  imports: [
    CommonModule,
    FormsModule,
    MatCardModule,
    MatButtonModule,
    MatSnackBarModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule
  ],
  templateUrl: './corporate-tax-center.component.html',
  styleUrls: ['./corporate-tax-center.component.scss']
})
export class CorporateTaxCenterComponent implements OnInit {

  filings: any[] = [];
  accounts: any[] = [];

  periodId = '';
  fromDate = '';
  toDate = '';

  selectedAccountId?: string;

  loading = false;

  constructor(
    private taxService: TaxService,
    private accountsService: AccountsService,
    private snackbar: MatSnackBar
  ) {}

  ngOnInit(): void {
    this.load();
    this.accountsService.list().subscribe(a => this.accounts = a);
  }

  load() {
    this.loading = true;
    this.taxService.listCorporate().subscribe({
      next: f => {
        this.filings = f;
        this.loading = false;
      },
      error: () => this.loading = false
    });
  }

  accrue() {
    if (!this.periodId || !this.fromDate || !this.toDate) {
      this.snackbar.open('All fields required', 'Close', { duration: 2000 });
      return;
    }

    this.taxService.accrueCorporate(
      this.periodId,
      new Date(this.fromDate).toISOString(),
      new Date(this.toDate).toISOString()
    ).subscribe({
      next: () => {
        this.snackbar.open('Corporate tax accrued', 'Close', { duration: 2000 });
        this.load();
      },
      error: () =>
        this.snackbar.open('Accrual failed', 'Close', { duration: 3000 })
    });
  }

  pay(filing: any) {
    if (!this.selectedAccountId) return;

    this.taxService.payCorporate(filing.id, this.selectedAccountId)
      .subscribe({
        next: () => {
          this.snackbar.open('Corporate tax paid', 'Close', { duration: 2000 });
          this.load();
        },
        error: () =>
          this.snackbar.open('Payment failed', 'Close', { duration: 3000 })
      });
  }
}