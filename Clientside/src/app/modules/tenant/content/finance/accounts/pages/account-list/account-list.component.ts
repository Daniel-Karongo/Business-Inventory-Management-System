import {
  Component,
  OnInit,
  computed,
  inject
} from '@angular/core';

import { CommonModule } from '@angular/common';

import { Router } from '@angular/router';

import { MatTableModule }
  from '@angular/material/table';

import { MatButtonModule }
  from '@angular/material/button';

import { MatIconModule }
  from '@angular/material/icon';

import { MatTooltipModule }
  from '@angular/material/tooltip';

import { EnterpriseTableComponent }
  from '../../../../../../../shared/components/enterprise-table/enterprise-table.component';

import { AccountsFacade }
  from '../../facades/accounts.facade';

import { Account }
  from '../../models/account.models';
import { WorkflowCardComponent } from '../../../../../../../shared/layout/workflow-card/workflow-card.component';
import { WorkflowShellComponent } from '../../../../../../../shared/layout/workflow-shell/workflow-shell.component';
import { EnterprisePaginatorComponent } from '../../../../../../../shared/components/enterprise-paginator/enterprise-paginator.component';

@Component({
  standalone: true,
  imports: [
    CommonModule,
    MatTableModule,
    MatButtonModule,
    MatIconModule,
    MatTooltipModule,
    EnterpriseTableComponent,
    WorkflowShellComponent,
    WorkflowCardComponent,
    EnterprisePaginatorComponent
  ],
  templateUrl:
    './account-list.component.html',
  styleUrls: [
    './account-list.component.scss'
  ]
})
export class AccountListComponent
  implements OnInit {

  private readonly facade =
    inject(AccountsFacade);

  private readonly router =
    inject(Router);

  readonly displayedColumns = [
    'code',
    'name',
    'type',
    'actions'
  ];

  readonly accounts =
    this.facade.data;

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

  readonly pageData =
    computed(() =>
      this.facade.page()
    );

  currentSize = 50;

  ngOnInit(): void {

    this.facade.load({
      page: 0,
      size: 50,
      sort: 'code'
    });
  }

  changeSize(
    size: number
  ): void {

    this.currentSize = size;

    this.facade.load({
      page: 0,
      size,
      sort: 'code'
    });
  }

  changePage(
    page: number
  ): void {

    this.facade.load({
      page,
      size: this.currentSize,
      sort: 'code'
    });
  }

  create(): void {

    this.router.navigate([
      '/app/finance/accounting/chart/create'
    ]);
  }

  edit(
    account: Account
  ): void {

    this.router.navigate([
      '/app/finance/accounting/chart',
      account.id
    ]);
  }

  trackByAccount(
    _: number,
    item: Account
  ): string {

    return item.id;
  }
}