import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { AccountsService } from '../../services/accounts.service';

@Component({
  standalone: true,
  imports: [CommonModule],
  templateUrl: './ledger-home.component.html',
  styleUrls: ['./ledger-home.component.scss']
})
export class LedgerHomeComponent implements OnInit {

  accounts: any[] = [];

  constructor(
    private accountsService: AccountsService,
    private router: Router
  ) {}

  ngOnInit() {
    this.accountsService.list().subscribe(a => this.accounts = a);
  }

  open(accountId: string) {
    this.router.navigate(['/accounts/ledger', accountId]);
  }
}