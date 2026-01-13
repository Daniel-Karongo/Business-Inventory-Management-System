import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';

import { MatTableModule } from '@angular/material/table';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';

import { AccountsService } from '../../services/accounts.service';

@Component({
  standalone: true,
  imports: [CommonModule, MatTableModule, MatButtonModule, MatIconModule],
  templateUrl: './account-list.component.html',
  styleUrls: ['./account-list.component.scss']
})
export class AccountListComponent implements OnInit {

  displayedColumns = ['code', 'name', 'type', 'status', 'actions'];
  accounts: any[] = [];

  constructor(
    private accountsService: AccountsService,
    private router: Router
  ) {}

  ngOnInit() {
    this.accountsService.list().subscribe(a => this.accounts = a);
  }

  create() {
    this.router.navigate(['/accounts/chart/create']);
  }

  edit(a: any) {
    this.router.navigate(['/accounts/chart', a.id]);
  }
}