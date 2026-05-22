import {
  Component,
  OnInit,
  computed,
  inject
} from '@angular/core';

import { CommonModule }
  from '@angular/common';

import {
  ActivatedRoute,
  Router
} from '@angular/router';

import { MatTableModule }
  from '@angular/material/table';

import { MatButtonModule }
  from '@angular/material/button';

import { MatIconModule }
  from '@angular/material/icon';

import {
  WorkflowShellComponent
} from '../../../../../../../shared/layout/workflow-shell/workflow-shell.component';

import {
  WorkflowCardComponent
} from '../../../../../../../shared/layout/workflow-card/workflow-card.component';

import {
  EnterpriseTableComponent
} from '../../../../../../../shared/components/enterprise-table/enterprise-table.component';

import { AccountsService }
  from '../../services/accounts.service';

import { LedgerFacade }
  from '../../facades/ledger.facade';

import { Account }
  from '../../models/account.models';
import { EnterprisePaginatorComponent } from '../../../../../../../shared/components/enterprise-paginator/enterprise-paginator.component';

@Component({
  standalone: true,
  imports: [
    CommonModule,
    MatTableModule,
    MatButtonModule,
    MatIconModule,
    WorkflowShellComponent,
    WorkflowCardComponent,
    EnterpriseTableComponent,
    EnterprisePaginatorComponent
  ],
  templateUrl:
    './ledger-view.component.html',
  styleUrls: [
    './ledger-view.component.scss'
  ]
})
export class LedgerViewComponent
  implements OnInit {

  private readonly route =
    inject(ActivatedRoute);

  private readonly router =
    inject(Router);

  private readonly accountsService =
    inject(AccountsService);

  private readonly facade =
    inject(LedgerFacade);

  readonly request =
    this.facade.request;

  readonly loading =
    computed(() =>
      this.request().loading
    );

  readonly error =
    computed(() =>
      this.request().error
    );

  readonly rows =
    this.facade.data;

  accountId!: string;

  account?: Account;

  readonly displayedColumns = [
    'date',
    'reference',
    'direction',
    'amount',
    'balance'
  ];

  readonly pageData =
    computed(() =>
      this.facade.page()
    );

  readonly currentSize =
    computed(() =>
      this.pageData().pageSize
      || 25
    );

  ngOnInit(): void {

    const id =
      this.route
        .snapshot
        .paramMap
        .get('accountId');

    if (!id) {
      return;
    }

    this.accountId = id;

    this.loadAccount();

    this.facade.load(
      this.accountId,
      {
        page: 0,
        size: this.currentSize(),
        sort: 'postedAt,desc'
      }
    );
  }

  private loadAccount(): void {

    this.accountsService
      .get(this.accountId)
      .subscribe({
        next: account => {
          this.account = account;
        }
      });
  }

  changePage(
    page: number
  ): void {

    if (!this.accountId) {
      return;
    }

    this.facade.load(
      this.accountId!,
      {
        page,
        size: this.currentSize(),
        sort: 'postedAt,desc'
      }
    );
  }

  changeSize(
    size: number
  ): void {

    if (!this.accountId) {
      return;
    }

    this.facade.load(
      this.accountId!,
      {
        page: 0,
        size,
        sort: 'postedAt,desc'
      }
    );
  }

  back(): void {

    this.router.navigate([
      '/app/finance/accounting/ledger'
    ]);
  }

  trackByRow(
    _: number,
    row: any
  ): string {

    return `${row.journalId}-${row.postedAt}`;
  }

}