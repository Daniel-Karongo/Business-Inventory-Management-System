import {
  Routes
} from '@angular/router';

import {
  VatCenterComponent
} from './pages/vat-center/vat-center.component';

import {
  CorporateTaxCenterComponent
} from './pages/corporate-tax-center/corporate-tax-center.component';

import {
  TaxAdministrationComponent
} from './pages/tax-administration/tax-administration.component';

import {
  TaxDashboardComponent
} from './pages/tax-dashboard/tax-dashboard.component';

export const TAX_ROUTES: Routes = [

  {
    path: '',
    component:
      TaxDashboardComponent
  },

  {
    path: 'vat',
    component:
      VatCenterComponent
  },

  {
    path: 'corporate',
    component:
      CorporateTaxCenterComponent
  },

  {
    path: 'administration',
    component:
      TaxAdministrationComponent
  }
];