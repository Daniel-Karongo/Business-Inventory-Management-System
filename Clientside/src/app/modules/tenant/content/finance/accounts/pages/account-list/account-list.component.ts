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
import { FormsModule } from '@angular/forms';
import { MatSelectModule } from '@angular/material/select';
import { BranchContextService } from '../../../../../../../core/services/branch-context.service';
import { toSignal } from '@angular/core/rxjs-interop';
import { MatInputModule } from '@angular/material/input';
import { MatChipsModule } from '@angular/material/chips';
import { MatButtonToggleModule } from '@angular/material/button-toggle';

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
    EnterprisePaginatorComponent,
    MatSelectModule,
    FormsModule,
    MatInputModule,
    MatChipsModule,
    MatButtonToggleModule
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

  private readonly branchContext =
    inject(BranchContextService);

  readonly displayedColumns = [
    'code',
    'name',
    'type',
    'balance',
    'updatedAt',
    'actions'
  ];

  readonly rawAccounts =
    this.facade.data;

  readonly accounts =
    computed(() =>
      this.rawAccounts()
    );

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

  tableState = {
    search: '',
    type: 'ALL',
    balanceFilter: 'ALL',
    status: 'ACTIVE',
    page: 0,
    size: 50,
    sort: 'code,asc'
  };

  currentSize = 50;

  readonly currentBranch =
    toSignal(
      this.branchContext.branch$,
      {
        initialValue: null
      }
    );

  readonly branches =
    toSignal(
      this.branchContext.branches$,
      {
        initialValue: []
      }
    );

  readonly accountTypes = [
    'ALL',
    'ASSET',
    'LIABILITY',
    'EQUITY',
    'INCOME',
    'EXPENSE'
  ];

  readonly balanceFilters = [
    'ALL',
    'POSITIVE',
    'NEGATIVE',
    'ZERO',
    'NON_ZERO'
  ];

  selectedBranchId: string | null =
    null;

  ngOnInit(): void {

    this.selectedBranchId =
      this.currentBranch();

    this.loadAccounts();
  }

  loadAccounts(): void {
    this.facade.load(
      {
        page: this.tableState.page,
        size: this.tableState.size,
        sort: this.tableState.sort,
        search:
          this.tableState.search,
        type:
          this.tableState.type !== 'ALL'
            ? this.tableState.type
            : undefined,
        active:
          this.tableState.status === 'ALL'
            ? undefined
            : this.tableState.status === 'ACTIVE',
        balanceFilter:
          this.tableState.balanceFilter !== 'ALL'
            ? this.tableState.balanceFilter
            : undefined
      },
      this.selectedBranchId ?? undefined
    );
  }

  sort(
    column: string
  ): void {
    const current =
      this.tableState.sort;

    const [
      currentColumn,
      currentDirection
    ] = current.split(',');

    const nextDirection =
      currentColumn === column &&
        currentDirection === 'asc'
        ? 'desc'
        : 'asc';

    this.tableState.sort =
      `${column},${nextDirection}`;

    this.loadAccounts();
  }

  readonly nonZeroAccountsCount =
    computed(
      () =>
        this.accounts()
          .filter(
            a => a.balance !== 0
          )
          .length
    );

  readonly totalBalance =
    computed(
      () =>
        this.accounts().reduce(
          (sum, a) =>
            sum + a.balance,
          0
        )
    );

  changeBranch(
    branchId: string
  ): void {

    this.selectedBranchId =
      branchId;

    this.loadAccounts();
  }

  changeSize(
    size: number
  ): void {

    this.currentSize = size;

    this.facade.load(
      {
        page: 0,
        size,
        sort: 'code'
      },
      this.selectedBranchId ?? undefined
    );
  }

  changePage(
    page: number
  ): void {

    this.facade.load(
      {
        page,
        size: this.currentSize,
        sort: 'code'
      },
      this.selectedBranchId ?? undefined
    );
  }

  create(): void {

    this.router.navigate([
      '/app/finance/accounting/chart/create'
    ]);
  }

  resetFilters(): void {

    this.tableState = {
      ...this.tableState,
      search: '',
      type: 'ALL',
      balanceFilter: 'ALL',
      status: 'ACTIVE',
      page: 0,
      sort: 'code,asc'
    };

    this.loadAccounts();
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