import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatCardModule } from '@angular/material/card';
import { MatButtonModule } from '@angular/material/button';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { MatSelectModule } from '@angular/material/select';
import { MatFormFieldModule } from '@angular/material/form-field';
import { FormsModule } from '@angular/forms';

import { TaxService } from '../../services/tax.service';
import { AccountsService } from '../../../../accounts/services/accounts.service';

@Component({
  standalone: true,
  selector: 'app-vat-center',
  imports: [
    CommonModule,
    MatCardModule,
    MatButtonModule,
    MatSnackBarModule,
    MatSelectModule,
    MatFormFieldModule,
    FormsModule
  ],
  templateUrl: './vat-center.component.html',
  styleUrls: ['./vat-center.component.scss']
})
export class VatCenterComponent implements OnInit {

  filings: any[] = [];
  accounts: any[] = [];

  loading = false;
  selectedAccountId?: string;
  search = '';

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

    this.taxService.listVat().subscribe({
      next: f => {
        this.filings = f;
        this.loading = false;
      },
      error: () => {
        this.snackbar.open('Failed to load VAT filings', 'Close', { duration: 3000 });
        this.loading = false;
      }
    });
  }

  pay(filing: any) {
    if (!this.selectedAccountId) return;

    this.taxService.payVat(filing.id, this.selectedAccountId)
      .subscribe({
        next: () => {
          this.snackbar.open('VAT payment successful', 'Close', { duration: 2000 });
          this.load();
        },
        error: () =>
          this.snackbar.open('VAT payment failed', 'Close', { duration: 3000 })
      });
  }

  filtered() {
    if (!this.search) return this.filings;

    return this.filings.filter(f =>
      f.period?.id?.toLowerCase().includes(this.search.toLowerCase())
    );
  }
}