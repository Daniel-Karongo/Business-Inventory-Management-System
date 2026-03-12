import { Routes } from '@angular/router';

import { AccountsDashboardComponent } from './accounts-dashboard/accounts-dashboard.component';
import { AccountCreateComponent } from './pages/account-create/account-create.component';
import { AccountEditComponent } from './pages/account-edit/account-edit.component';
import { AccountListComponent } from './pages/account-list/account-list.component';
import { JournalDetailsComponent } from './pages/journal-details/journal-details.component';
import { JournalListComponent } from './pages/journal-list/journal-list.component';
import { LedgerHomeComponent } from './pages/ledger-home/ledger-home.component';
import { LedgerViewComponent } from './pages/ledger-view/ledger-view.component';
import { ManualJournalComponent } from './pages/manual-journal/manual-journal.component';
import { roleAtLeast } from '../../../../../core/security/role-at-least.guard';

export const ACCOUNTS_ROUTES: Routes = [
  {
    path: '',
    canMatch: [roleAtLeast('MANAGER')],
    children: [

      { path: '', component: AccountsDashboardComponent },

      { path: 'chart', component: AccountListComponent },
      { path: 'chart/create', component: AccountCreateComponent },
      { path: 'chart/:id', component: AccountEditComponent },

      { path: 'journals/new', component: ManualJournalComponent },
      { path: 'journals', component: JournalListComponent },
      { path: 'journals/:id', component: JournalDetailsComponent },

      { path: 'ledger', component: LedgerHomeComponent },
      { path: 'ledger/:accountId', component: LedgerViewComponent }

    ]
  }
];