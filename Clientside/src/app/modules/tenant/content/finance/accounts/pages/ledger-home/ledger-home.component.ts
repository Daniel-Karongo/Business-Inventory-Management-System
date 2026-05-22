import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';

import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';

import { AccountsService } from '../../services/accounts.service';

import { Account } from '../../models/account.models';

import { EnterpriseTableComponent } from '../../../../../../../shared/components/enterprise-table/enterprise-table.component';

@Component({
  standalone: true,
  imports: [
    CommonModule,
    MatButtonModule,
    MatIconModule,
    EnterpriseTableComponent
  ],
  templateUrl: './ledger-home.component.html',
  styleUrls: ['./ledger-home.component.scss']
})
export class LedgerHomeComponent implements OnInit {

  accounts: Account[] = [];

  loading = false;

  constructor(
    private accountsService: AccountsService,
    private router: Router
  ) { }

  ngOnInit(): void {
    this.load();
  }

  load(): void {

    this.loading = true;

    this.accountsService
      .list({
        page: 0,
        size: 500,
        sort: 'code'
      })
      .subscribe({
        next: res => {
          this.accounts = res.content ?? [];
          this.loading = false;
        },
        error: () => {
          this.loading = false;
        }
      });
  }

  open(accountId: string): void {
    this.router.navigate([
      '/app/finance/accounting/ledger',
      accountId
    ]);
  }

  trackByAccount(
    _: number,
    item: Account
  ): string {

    return item.id;
  }
}