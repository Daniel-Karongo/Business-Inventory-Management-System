import {
  Component,
  OnDestroy,
  OnInit,
  inject
} from '@angular/core';
import {
  CommonModule
} from '@angular/common';
import {
  FormsModule
} from '@angular/forms';
import {
  Subject,
  takeUntil,
  forkJoin,
  of,
  catchError
} from 'rxjs';
import {
  MatButtonModule
} from '@angular/material/button';
import {
  MatIconModule
} from '@angular/material/icon';
import {
  MatFormFieldModule
} from '@angular/material/form-field';
import {
  MatInputModule
} from '@angular/material/input';
import {
  MatSelectModule
} from '@angular/material/select';
import {
  MatSnackBar,
  MatSnackBarModule
} from '@angular/material/snack-bar';
import {
  WorkflowShellComponent
} from '../../../../../../../shared/layout/workflow-shell/workflow-shell.component';
import {
  BranchService
} from '../../../../branches/services/branch.service';
import {
  BranchListItemDTO
} from '../../../../branches/models/branch.model';
import {
  BranchContextService
} from '../../../../../../../core/services/branch-context.service';
import {
  TaxService
} from '../../services/tax.service';
import {
  CorporateTaxFiling,
  PageResponse,
  TaxPeriod,
  TaxStatus
} from '../../models/tax.models';
import { FundingAccount } from '../../../ap/debts/models/funding-account.model';

@Component({
  standalone: true,
  selector: 'app-corporate-tax-center',
  templateUrl: './corporate-tax-center.component.html',
  styleUrls: ['./corporate-tax-center.component.scss'],
  imports: [
    CommonModule,
    FormsModule,
    MatButtonModule,
    MatIconModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatSnackBarModule,
    WorkflowShellComponent
  ]
})
export class CorporateTaxCenterComponent
  implements OnInit, OnDestroy {

  private readonly destroy$ =
    new Subject<void>();

  private readonly taxService =
    inject(TaxService);

  private readonly branchService =
    inject(BranchService);

  private readonly branchContext =
    inject(BranchContextService);

  private readonly snackbar =
    inject(MatSnackBar);

  loading = true;

  accruing = false;

  selectedBranchId:
    string | null = null;

  branches:
    BranchListItemDTO[] = [];

  status:
    TaxStatus | null = null;

  filings:
    CorporateTaxFiling[] = [];

  periods:
    TaxPeriod[] = [];

  fundingAccounts: FundingAccount[] = [];

  accountId = '';

  periodId = '';

  fromDate = '';

  toDate = '';

  activeTab:
    'overview'
    | 'filings'
    = 'overview';

  readonly tabs: {
    key: 'overview' | 'filings';
    label: string;
    icon: string;
  }[] = [
      {
        key: 'overview',
        label: 'Overview',
        icon: 'dashboard'
      },
      {
        key: 'filings',
        label: 'Filings',
        icon: 'account_balance'
      }
    ];

  ngOnInit(): void {

    this.loadBranches();

    this.branchContext.branch$
      .pipe(
        takeUntil(this.destroy$)
      )
      .subscribe(branchId => {

        this.selectedBranchId =
          branchId;

        if (branchId) {

          this.loadWorkspace(
            branchId
          );
        }
      });
  }

  ngOnDestroy(): void {

    this.destroy$.next();

    this.destroy$.complete();
  }

  private createEmptyPageResponse<T>():
    PageResponse<T> {

    return {
      content: [],
      totalElements: 0,
      totalPages: 0,
      number: 0,
      size: 100,
      first: true,
      last: true
    };
  }

  private loadBranches() {

    this.branchService
      .getAllLegacy()
      .pipe(
        takeUntil(this.destroy$)
      )
      .subscribe({
        next: branches => {

          this.branches =
            branches ?? [];

          this.branchContext
            .setBranches(
              this.branches
            );

          const current =
            this.branchContext
              .currentBranch;

          if (
            current &&
            !this.selectedBranchId
          ) {

            this.selectedBranchId =
              current;
          }

          if (
            !this.selectedBranchId &&
            this.branches.length === 1
          ) {

            this.selectedBranchId =
              this.branches[0].id;

            this.branchContext
              .setBranch(
                this.branches[0].id
              );
          }

          if (
            !this.selectedBranchId
          ) {

            this.loading = false;
          }
        },
        error: () => {

          this.branches = [];
          this.loading = false;
        }
      });
  }

  loadWorkspace(
    branchId: string | null
  ) {

    if (!branchId) {

      this.loading = false;
      return;
    }

    this.loading = true;

    forkJoin({
      status: this.taxService
        .getStatus(branchId)
        .pipe(
          catchError(() =>
            of(null)
          )
        ),

      periods: this.taxService
        .listPeriods(
          0,
          100,
          branchId
        )
        .pipe(
          catchError(() =>
            of(
              this.createEmptyPageResponse<TaxPeriod>()
            )
          )
        ),

      filings: this.taxService
        .listCorporate(
          0,
          100,
          branchId
        )
        .pipe(
          catchError(() =>
            of(
              this.createEmptyPageResponse<CorporateTaxFiling>()
            )
          )
        ),

      fundingAccounts: this.taxService
        .getFundingAccounts(
          branchId
        )
        .pipe(
          catchError(() =>
            of([])
          )
        )
    })
      .pipe(
        takeUntil(
          this.destroy$
        )
      )
      .subscribe({
        next: result => {

          this.status =
            result?.status ?? null;

          this.periods =
            result?.periods?.content ?? [];

          this.filings =
            result?.filings?.content ?? [];

          const fundingAccounts =
            result?.fundingAccounts ?? [];

          if (
            this.accountId &&
            !fundingAccounts.some(
              x => x.id === this.accountId
            )
          ) {
            this.accountId = '';
          }

          this.fundingAccounts =
            fundingAccounts;

          if (
            this.periodId &&
            !this.periods.some(
              x => x.id === this.periodId
            )
          ) {

            this.periodId = '';
          }

          if (
            this.accountId &&
            !this.fundingAccounts.some(
              x => x.id === this.accountId
            )
          ) {

            this.accountId = '';
          }

          this.loading = false;
        },

        error: () => {

          this.status = null;
          this.periods = [];
          this.filings = [];
          this.fundingAccounts = [];
          this.periodId = '';
          this.accountId = '';

          this.loading = false;
        }
      });
  }

  onBranchChanged(
    branchId: string
  ) {

    this.branchContext
      .setBranch(
        branchId
      );
  }

  selectTab(
    key:
      'overview'
      | 'filings'
  ) {

    this.activeTab =
      key;
  }

  accrueCorporate() {

    if (!this.periodId) {

      this.snackbar.open(
        'Please select a tax period.',
        'Close',
        {
          duration: 3000
        }
      );

      return;
    }

    this.accruing = true;

    this.taxService
      .accrueCorporate(
        this.periodId,
        this.selectedBranchId ?? undefined
      )
      .subscribe({
        next: () => {

          this.accruing = false;

          this.snackbar.open(
            'Corporate tax accrued successfully',
            'Close',
            {
              duration: 2500
            }
          );

          this.loadWorkspace(
            this.selectedBranchId
          );
        },

        error: () => {

          this.accruing = false;

          this.snackbar.open(
            'Failed to accrue corporate tax',
            'Close',
            {
              duration: 3000
            }
          );
        }
      });
  }

  payCorporate(
    filingId: string
  ) {

    if (!this.accountId) {

      this.snackbar.open(
        'Account ID is required',
        'Close',
        {
          duration: 3000
        }
      );

      return;
    }

    this.taxService
      .payCorporate(
        filingId,
        this.accountId
      )
      .subscribe({
        next: () => {

          this.snackbar.open(
            'Corporate tax payment recorded',
            'Close',
            {
              duration: 2500
            }
          );

          this.loadWorkspace(
            this.selectedBranchId
          );
        },

        error: () => {

          this.snackbar.open(
            'Failed to record payment',
            'Close',
            {
              duration: 3000
            }
          );
        }
      });
  }

  get currentBranch() {

    return this.branches.find(
      x =>
        x.id ===
        this.selectedBranchId
    );
  }

  get branchSelectorVisible() {

    return this.branches.length > 1;
  }

  get hasPeriods() {

    return this.periods.length > 0;
  }

  get hasFilings() {

    return this.filings.length > 0;
  }

  get hasTaxActivity() {

    return (
      this.hasPeriods ||
      this.hasFilings
    );
  }

  get showEmptyState() {

    return !this.hasTaxActivity;
  }

  get canAccrueCorporate() {

    return (
      !!this.periodId &&
      this.hasPeriods &&
      !this.accruing &&
      !this.status?.locked
    );
  }

  get selectedAccount() {
    return this.fundingAccounts.find(
      x => x.id === this.accountId
    );
  }
}