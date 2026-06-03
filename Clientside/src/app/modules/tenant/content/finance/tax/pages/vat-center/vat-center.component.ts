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
  AccountingPeriod,
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
    AccountingPeriod | null = null;

  filings:
    VatFiling[] = [];

  branches:
    BranchListItemDTO[] = [];

  fundingAccounts: FundingAccount[] = [];

  periods: AccountingPeriod[] = [];
  selectedPeriodId = '';
  eligibleVatPeriods: AccountingPeriod[] = [];

  requestingRefund = false;
  completingRefund = false;

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

      periods: this.taxService
        .listPeriods(
          0,
          100,
          branchId
        )
        .pipe(
          catchError(() =>
            of(
              this.createEmptyPageResponse<AccountingPeriod>()
            )
          )
        ),

      currentPeriod: this.taxService
        .getCurrentPeriod(branchId)
        .pipe(
          catchError(() => of(null))
        ),

      eligiblePeriods: this.taxService
        .getEligibleVatPeriods(
          branchId
        )
        .pipe(
          catchError(() =>
            of([])
          )
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

          this.periods =
            result?.periods?.content ?? [];

          this.currentPeriod =
            result?.currentPeriod ?? null;

          this.eligibleVatPeriods =
            result?.eligiblePeriods ?? [];

          this.filings =
            result?.filings?.content ?? [];

          if (
            this.selectedPeriodId &&
            !this.eligibleVatPeriods.some(
              p => p.id === this.selectedPeriodId
            )
          ) {
            this.selectedPeriodId = '';
          }

          if (
            !this.selectedPeriodId &&
            this.eligibleVatPeriods.length > 0
          ) {
            this.selectedPeriodId =
              this.eligibleVatPeriods[0].id;
          }

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

          this.periods = [];
          this.eligibleVatPeriods = [];

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
      !this.selectedPeriodId ||
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
        this.selectedPeriodId,
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

        error: err => {

          this.filing = false;

          this.snackbar.open(
            err?.error?.message ??
            'Failed to file VAT',
            'Close',
            {
              duration: 5000
            }
          );
        }
      });
  }

  requestRefund(
    filingId: string
  ) {
    this.requestingRefund = true;

    this.taxService
      .requestVatRefund(
        filingId
      )
      .subscribe({
        next: () => {
          this.requestingRefund = false;

          this.snackbar.open(
            'VAT refund requested',
            'Close',
            {
              duration: 3000
            }
          );

          this.loadWorkspace(
            this.selectedBranchId
          );
        },
        error: err => {
          this.requestingRefund = false;

          this.snackbar.open(
            err?.error?.message ??
            'Failed to request refund',
            'Close',
            {
              duration: 5000
            }
          );
        }
      });
  }

  completeRefund(
    filingId: string
  ) {
    if (!this.accountId) {
      this.snackbar.open(
        'Receiving account required',
        'Close',
        {
          duration: 3000
        }
      );
      return;
    }

    this.completingRefund = true;

    this.taxService
      .completeVatRefund(
        filingId,
        this.accountId
      )
      .subscribe({
        next: () => {
          this.completingRefund = false;

          this.snackbar.open(
            'VAT refund recorded',
            'Close',
            {
              duration: 3000
            }
          );

          this.loadWorkspace(
            this.selectedBranchId
          );
        },
        error: err => {
          this.completingRefund = false;

          this.snackbar.open(
            err?.error?.message ??
            'Failed to complete refund',
            'Close',
            {
              duration: 5000
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

        error: err => {

          this.paying = false;

          this.snackbar.open(
            err?.error?.message ??
            'Failed to record payment',
            'Close',
            {
              duration: 5000
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
      !!this.selectedPeriodId &&
      !this.filing
    );
  }

  get selectedAccount() {
    return this.fundingAccounts.find(
      x => x.id === this.accountId
    );
  }

  get hasEligibleVatPeriods() {
    return (
      this.eligibleVatPeriods.length > 0
    );
  }

  isPayable(
    filing: VatFiling
  ) {
    return (
      filing.status ===
      'VAT_PAYABLE'
    );
  }

  isCredit(
    filing: VatFiling
  ) {
    return (
      filing.status ===
      'VAT_CREDIT_AVAILABLE' ||

      filing.status ===
      'VAT_CREDIT_CARRIED_FORWARD'
    );
  }

  isRefundPending(
    filing: VatFiling
  ) {
    return (
      filing.status ===
      'VAT_REFUND_PENDING'
    );
  }

  isRefunded(
    filing: VatFiling
  ) {
    return (
      filing.status ===
      'VAT_REFUNDED'
    );
  }

  statusLabel(
    filing: VatFiling
  ): string {

    switch (filing.status) {

      case 'VAT_PAYABLE':
        return 'VAT Payable';

      case 'PAID':
        return 'Paid';

      case 'VAT_CREDIT_AVAILABLE':
        return 'Credit Available';

      case 'VAT_CREDIT_CARRIED_FORWARD':
        return 'Credit Carried Forward';

      case 'VAT_REFUND_PENDING':
        return 'Refund Pending';

      case 'VAT_REFUNDED':
        return 'Refunded';

      default:
        return filing.status;
    }
  }
}