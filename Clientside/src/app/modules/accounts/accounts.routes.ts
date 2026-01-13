import { Routes } from '@angular/router';

import { AccountListComponent } from './pages/account-list/account-list.component';
import { AccountCreateComponent } from './pages/account-create/account-create.component';
import { ManualJournalComponent } from './pages/manual-journal/manual-journal.component';
import { JournalListComponent } from './pages/journal-list/journal-list.component';
import { JournalDetailsComponent } from './pages/journal-details/journal-details.component';
import { LedgerHomeComponent } from './pages/ledger-home/ledger-home.component';
import { LedgerViewComponent } from './pages/ledger-view/ledger-view.component';
import { AccountsDashboardComponent } from './accounts-dashboard/accounts-dashboard.component';
import { AccountEditComponent } from './pages/account-edit/account-edit.component';

export const ACCOUNTS_ROUTES: Routes = [
  { path: '', component: AccountsDashboardComponent },

  // Chart of Accounts
  { path: 'chart', component: AccountListComponent },
  { path: 'chart/create', component: AccountCreateComponent },
  { path: 'chart/:id', component: AccountEditComponent },

  // Journals
  { path: 'journals/new', component: ManualJournalComponent },
  { path: 'journals', component: JournalListComponent },
  { path: 'journals/:id', component: JournalDetailsComponent },

  // Ledger
  { path: 'ledger', component: LedgerHomeComponent },
  { path: 'ledger/:accountId', component: LedgerViewComponent }
];