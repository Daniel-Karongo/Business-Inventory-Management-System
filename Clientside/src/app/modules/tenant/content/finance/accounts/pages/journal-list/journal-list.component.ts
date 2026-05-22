import {
  Component,
  OnInit,
  computed,
  inject
} from '@angular/core';

import { CommonModule }
  from '@angular/common';

import { Router }
  from '@angular/router';

import { MatTableModule }
  from '@angular/material/table';

import { MatIconModule }
  from '@angular/material/icon';

import { MatButtonModule }
  from '@angular/material/button';

import { MatTooltipModule }
  from '@angular/material/tooltip';

import {
  WorkflowShellComponent
} from '../../../../../../../shared/layout/workflow-shell/workflow-shell.component';

import {
  WorkflowCardComponent
} from '../../../../../../../shared/layout/workflow-card/workflow-card.component';

import {
  EnterpriseTableComponent
} from '../../../../../../../shared/components/enterprise-table/enterprise-table.component';

import { JournalFacade }
  from '../../facades/journal.facade';

import { Journal }
  from '../../models/journal.models';
import { EnterprisePaginatorComponent } from '../../../../../../../shared/components/enterprise-paginator/enterprise-paginator.component';

@Component({
  standalone: true,
  imports: [
    CommonModule,
    MatTableModule,
    MatIconModule,
    MatButtonModule,
    MatTooltipModule,
    WorkflowShellComponent,
    WorkflowCardComponent,
    EnterpriseTableComponent,
    EnterprisePaginatorComponent
  ],
  templateUrl:
    './journal-list.component.html',
  styleUrls: [
    './journal-list.component.scss'
  ]
})
export class JournalListComponent
  implements OnInit {

  private readonly facade =
    inject(JournalFacade);

  private readonly router =
    inject(Router);

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

  readonly journals =
    this.facade.data;

  readonly displayedColumns = [
    'reference',
    'description',
    'source',
    'postedAt',
    'status',
    'actions'
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

    this.facade.load({
      page: 0,
      size: this.currentSize(),
      sort: 'postedAt,desc'
    });
  }

  changePage(
    page: number
  ): void {

    this.facade.load({
      page,
      size: this.currentSize(),
      sort: 'postedAt,desc'
    });
  }

  changeSize(
    size: number
  ): void {

    this.facade.load({
      page: 0,
      size,
      sort: 'postedAt,desc'
    });
  }

  create(): void {

    this.router.navigate([
      '/app/finance/accounting/journals/new'
    ]);
  }

  view(
    journal: Journal
  ): void {

    this.router.navigate([
      '/app/finance/accounting/journals',
      journal.id
    ]);
  }

  trackByJournal(
    _: number,
    item: Journal
  ): string {

    return item.id;
  }

}