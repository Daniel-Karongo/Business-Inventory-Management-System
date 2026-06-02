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
  catchError,
  forkJoin,
  of,
  Subject,
  takeUntil
} from 'rxjs';
import {
  MatButtonModule
} from '@angular/material/button';
import {
  MatIconModule
} from '@angular/material/icon';
import {
  MatSnackBar,
  MatSnackBarModule
} from '@angular/material/snack-bar';
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
  WorkflowShellComponent
} from '../../../../../../../shared/layout/workflow-shell/workflow-shell.component';
import {
  TaxService
} from '../../services/tax.service';
import {
  PageResponse,
  TaxPeriod,
  TaxStatus,
  VatFiling
} from '../../models/tax.models';
import {
  BranchService
} from '../../../../branches/services/branch.service';
import {
  BranchListItemDTO
} from '../../../../branches/models/branch.model';
import {
  BranchContextService
} from '../../../../../../../core/services/branch-context.service';
import { FundingAccount } from '../../../ap/debts/models/funding-account.model';

@Component({
  standalone: true,
  selector: 'app-vat-center',
  templateUrl: './vat-center.component.html',
  styleUrls: ['./vat-center.component.scss'],
  imports: [
    CommonModule,
    FormsModule,
    MatButtonModule,
    MatIconModule,
    MatSnackBarModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    WorkflowShellComponent
  ]
})
export class VatCenterComponent
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

  filing = false;

  paying = false;

  status:
    TaxStatus | null = null;

  currentPeriod:
    TaxPeriod | null = null;

  filings:
    VatFiling[] = [];

  branches:
    BranchListItemDTO[] = [];

  fundingAccounts: FundingAccount[] = [];

  selectedBranchId = '';

  accountId = '';

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
        icon: 'receipt_long'
      }
    ];

  ngOnInit(): void {

    this.loadBranches();

    this.branchContext.branch$
      .pipe(
        takeUntil(this.destroy$)
      )
      .subscribe(branchId => {

        if (!branchId) {
          return;
        }

        this.selectedBranchId =
          branchId;

        this.loadWorkspace(
          branchId
        );
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

          const current =
            this.branchContext.currentBranchId();

          this.selectedBranchId =
            current
            ?? this.branches[0]?.id
            ?? '';

          if (
            this.selectedBranchId
          ) {

            this.branchContext.setBranch(
              this.selectedBranchId
            );

            this.loadWorkspace(
              this.selectedBranchId
            );
          } else {

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
    branchId: string
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
          catchError(() => of(null))
        ),

      period: this.taxService
        .getCurrentPeriod(branchId)
        .pipe(
          catchError(() => of(null))
        ),

      filings: this.taxService
        .listVat(
          0,
          100,
          branchId
        )
        .pipe(
          catchError(() =>
            of(
              this.createEmptyPageResponse<VatFiling>()
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

          this.currentPeriod =
            result?.period ?? null;

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
          this.currentPeriod = null;
          this.filings = [];
          this.fundingAccounts = [];
          this.accountId = '';

          this.loading = false;
        }
      });
  }

  onBranchChanged(
    branchId: string
  ) {

    if (!branchId) {
      return;
    }

    this.branchContext.setBranch(
      branchId
    );
  }

  selectTab(
    key:
      'overview'
      | 'filings'
  ) {

    this.activeTab = key;
  }

  fileVat() {

    if (
      !this.currentPeriod ||
      !this.selectedBranchId
    ) {

      this.snackbar.open(
        'No active tax period available.',
        'Close',
        {
          duration: 3000
        }
      );

      return;
    }

    this.filing = true;

    this.taxService
      .fileVat(
        this.currentPeriod.id,
        this.selectedBranchId
      )
      .subscribe({
        next: () => {

          this.filing = false;

          this.snackbar.open(
            'VAT return filed',
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

          this.filing = false;

          this.snackbar.open(
            'Failed to file VAT',
            'Close',
            {
              duration: 3000
            }
          );
        }
      });
  }

  payVat(
    filingId: string
  ) {

    if (
      !this.accountId ||
      !this.selectedBranchId
    ) {

      this.snackbar.open(
        'Payment account is required.',
        'Close',
        {
          duration: 3000
        }
      );

      return;
    }

    this.paying = true;

    this.taxService
      .payVat(
        filingId,
        this.accountId,
        this.selectedBranchId
      )
      .subscribe({
        next: () => {

          this.paying = false;

          this.snackbar.open(
            'VAT payment recorded',
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

          this.paying = false;

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

  get branchSelectorVisible() {

    return this.branches.length > 1;
  }

  get currentBranch() {

    return this.branches.find(
      x =>
        x.id ===
        this.selectedBranchId
    );
  }

  get hasFilings() {

    return this.filings.length > 0;
  }

  get hasCurrentPeriod() {

    return !!this.currentPeriod;
  }

  get hasTaxActivity() {

    return (
      this.hasFilings ||
      this.hasCurrentPeriod
    );
  }

  get showEmptyState() {

    return !this.hasTaxActivity;
  }

  get canFileVat() {

    return (
      !!this.currentPeriod &&
      !this.status?.locked &&
      !this.filing
    );
  }

  get selectedAccount() {
    return this.fundingAccounts.find(
      x => x.id === this.accountId
    );
  }
}