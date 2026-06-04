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
  forkJoin,
  of,
  takeUntil,
  catchError
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
  TaxStatus,
  AccountingPeriod,
  VatDashboard,
  VatOverview,
  VatFilingSummary,
  VatFilingPreview,
  VatFilingReadiness,
  VatFilingDetail,
  VatRefund,
  VatCreditMovement,
  RecordVatPaymentRequest,
  VatPayment
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

import {
  FundingAccount
} from '../../../ap/debts/models/funding-account.model';

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

  requestingRefund = false;

  completingRefund = false;

  status:
    TaxStatus | null = null;

  dashboard:
    VatDashboard | null = null;

  overview:
    VatOverview | null = null;

  branches:
    BranchListItemDTO[] = [];

  fundingAccounts:
    FundingAccount[] = [];

  eligibleVatPeriods:
    AccountingPeriod[] = [];

  filings:
    VatFilingSummary[] = [];

  refunds:
    VatRefund[] = [];

  creditHistory:
    VatCreditMovement[] = [];

  selectedBranchId = '';

  selectedPeriodId = '';

  selectedAccountId = '';

  paymentAmount: number | null =
    null;

  preview:
    VatFilingPreview | null = null;

  readiness:
    VatFilingReadiness | null =
    null;

  selectedFiling:
    VatFilingDetail | null = null;

  activeFilingId = '';

  filingPayments: VatPayment[] = [];

  showFilingDetail = false;

  activeTab:
    | 'overview'
    | 'filings'
    | 'refunds'
    | 'credits'
    = 'overview';

  readonly tabs: {
    key:
    | 'overview'
    | 'filings'
    | 'refunds'
    | 'credits';
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
        label: 'VAT Returns',
        icon: 'receipt_long'
      },
      {
        key: 'refunds',
        label: 'Refunds',
        icon: 'request_quote'
      },
      {
        key: 'credits',
        label: 'Credit Activity',
        icon: 'account_balance_wallet'
      }
    ];

  ngOnInit(): void {

    this.loadBranches();

    this.branchContext.branch$
      .pipe(
        takeUntil(
          this.destroy$
        )
      )
      .subscribe(
        branchId => {

          if (!branchId) {
            return;
          }

          this.selectedBranchId =
            branchId;

          this.loadWorkspace(
            branchId
          );
        }
      );
  }

  ngOnDestroy(): void {

    this.destroy$.next();

    this.destroy$.complete();
  }

  loadBranches() {

    this.branchService
      .getAllLegacy()
      .pipe(
        takeUntil(
          this.destroy$
        )
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
          }
        }
      });
  }

  loadWorkspace(
    branchId: string
  ) {

    this.loading = true;

    this.showFilingDetail = false;
    this.selectedFiling = null;
    this.filingPayments = [];

    forkJoin({

      status:
        this.taxService
          .getStatus(branchId)
          .pipe(
            catchError(err => {

              this.snackbar.open(
                err?.error?.message ??
                'Unable to load VAT status',
                'Close',
                { duration: 5000 }
              );

              return of(null);
            })
          ),

      dashboard:
        this.taxService
          .getVatDashboard(branchId)
          .pipe(
            catchError(err => {

              this.snackbar.open(
                err?.error?.message ??
                'Unable to load VAT dashboard',
                'Close',
                { duration: 5000 }
              );

              return of(null);
            })
          ),

      overview:
        this.taxService
          .getVatOverview(branchId)
          .pipe(
            catchError(err => {

              this.snackbar.open(
                err?.error?.message ??
                'Unable to load VAT overview',
                'Close',
                { duration: 5000 }
              );

              return of(null);
            })
          ),

      periods:
        this.taxService
          .getEligibleVatPeriods(branchId)
          .pipe(
            catchError(err => {

              this.snackbar.open(
                err?.error?.message ??
                'Unable to load VAT periods',
                'Close',
                { duration: 5000 }
              );

              return of([]);
            })
          ),

      history:
        this.taxService
          .getVatHistory(branchId)
          .pipe(
            catchError(err => {

              this.snackbar.open(
                err?.error?.message ??
                'Unable to load VAT filing history',
                'Close',
                { duration: 5000 }
              );

              return of([]);
            })
          ),

      refunds:
        this.taxService
          .getVatRefunds(branchId)
          .pipe(
            catchError(err => {

              this.snackbar.open(
                err?.error?.message ??
                'Unable to load VAT refunds',
                'Close',
                { duration: 5000 }
              );

              return of([]);
            })
          ),

      credits:
        this.taxService
          .getVatCreditHistory(branchId)
          .pipe(
            catchError(err => {

              this.snackbar.open(
                err?.error?.message ??
                'Unable to load VAT credit history',
                'Close',
                { duration: 5000 }
              );

              return of([]);
            })
          ),

      fundingAccounts:
        this.taxService
          .getFundingAccounts(branchId)
          .pipe(
            catchError(err => {

              this.snackbar.open(
                err?.error?.message ??
                'Unable to load funding accounts',
                'Close',
                { duration: 5000 }
              );

              return of([]);
            })
          )

    })
      .pipe(
        takeUntil(this.destroy$)
      )
      .subscribe({

        next: result => {

          try {

            this.status =
              result.status;

            this.dashboard =
              result.dashboard;

            this.overview =
              result.overview;

            this.eligibleVatPeriods =
              result.periods ?? [];

            this.filings =
              result.history ?? [];

            this.refunds =
              result.refunds ?? [];

            this.creditHistory =
              result.credits ?? [];

            this.fundingAccounts =
              result.fundingAccounts ?? [];

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

              this.loadPreview();
            }

          } catch (error) {

            console.error(error);

            this.snackbar.open(
              'VAT workspace contains invalid data returned by the server.',
              'Close',
              {
                duration: 7000
              }
            );

          } finally {

            this.loading = false;

          }

        },

        error: err => {

          this.loading = false;

          this.snackbar.open(
            err?.error?.message ??
            'Unable to load VAT workspace',
            'Close',
            {
              duration: 7000
            }
          );
        }
      });
  }

  openFiling(
    filingId: string
  ) {

    this.activeFilingId =
      filingId;

    forkJoin({

      detail:
        this.taxService
          .getVatDetail(filingId),

      payments:
        this.taxService
          .getVatPayments(filingId)

    })
      .subscribe({

        next: result => {

          this.selectedFiling =
            result.detail;

          this.filingPayments =
            result.payments ?? [];

          this.showFilingDetail =
            true;
        },

        error: err => {

          this.snackbar.open(
            err?.error?.message ??
            'Unable to load VAT filing details',
            'Close',
            {
              duration: 5000
            }
          );
        }
      });
  }

  loadPreview() {

    if (
      !this.selectedPeriodId
    ) {
      return;
    }

    forkJoin({

      readiness:
        this.taxService
          .getVatReadiness(
            this.selectedPeriodId,
            this.selectedBranchId
          ),

      preview:
        this.taxService
          .getVatPreview(
            this.selectedPeriodId,
            this.selectedBranchId
          )

    }).subscribe({
      next: result => {

        this.readiness =
          result.readiness ?? {
            ready: false,
            message: 'Preview unavailable',
            warnings: []
          };

        this.preview =
          result.preview;
      },
      error: err => {

        this.snackbar.open(
          err?.error?.message ??
          'Unable to load VAT preview',
          'Close',
          {
            duration: 5000
          }
        );

      }
    });
  }

  fileVat() {

    if (
      !this.selectedPeriodId
    ) {
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
            'VAT return filed successfully',
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

          this.filing = false;

          this.snackbar.open(
            err?.error?.message ??
            'Failed to file VAT',
            'Close'
          );
        }
      });
  }

  loadDetail(
    filingId: string
  ) {

    this.taxService
      .getVatDetail(
        filingId
      )
      .subscribe(
        detail =>
          this.selectedFiling =
          detail
      );
  }

  payVat(
    filingId: string,
    outstandingAmount: number
  ) {

    if (
      !this.selectedAccountId
    ) {
      return;
    }

    const amount =
      this.paymentAmount
      ?? outstandingAmount;

    const payload:
      RecordVatPaymentRequest = {

      fundingAccountId:
        this.selectedAccountId,

      amount
    };

    this.paying = true;

    this.taxService
      .recordVatPayment(
        filingId,
        payload
      )
      .subscribe({

        next: () => {

          this.paying = false;

          this.paymentAmount =
            null;

          this.loadWorkspace(
            this.selectedBranchId
          );
        },

        error: err => {

          this.paying = false;

          this.snackbar.open(
            err?.error?.message ??
            'Failed to record payment',
            'Close'
          );
        }
      });
  }

  requestRefund(
    filingId: string
  ) {

    this.requestingRefund =
      true;

    this.taxService
      .requestVatRefund(filingId)
      .subscribe({

        next: () => {

          this.requestingRefund =
            false;

          this.snackbar.open(
            'Refund request submitted successfully.',
            'Close',
            {
              duration: 4000
            }
          );

          this.loadWorkspace(
            this.selectedBranchId
          );
        },

        error: err => {

          this.requestingRefund =
            false;

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

    if (!this.selectedAccountId) {

      this.snackbar.open(
        'Select a funding account first.',
        'Close',
        {
          duration: 4000
        }
      );

      return;
    }

    this.completingRefund =
      true;

    this.taxService
      .completeVatRefund(
        filingId,
        this.selectedAccountId
      )
      .subscribe({

        next: () => {

          this.completingRefund =
            false;

          this.snackbar.open(
            'Refund completed successfully.',
            'Close',
            {
              duration: 4000
            }
          );

          this.loadWorkspace(
            this.selectedBranchId
          );
        },

        error: err => {

          this.completingRefund =
            false;

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

  statusClass(
    status: string
  ): string {

    switch (status) {

      case 'Payment Due':
        return 'status-payable';

      case 'Partially Paid':
        return 'status-payable';

      case 'Paid':
        return 'status-paid';

      case 'Credit Available':
        return 'status-credit';

      case 'Refund Requested':
        return 'status-refund-pending';

      case 'Refunded':
        return 'status-refunded';

      default:
        return '';
    }
  }

  onBranchChanged(
    branchId: string
  ) {

    this.branchContext.setBranch(
      branchId
    );
  }

  selectTab(
    tab:
      | 'overview'
      | 'filings'
      | 'refunds'
      | 'credits'
  ) {

    this.activeTab =
      tab;
  }

  get branchSelectorVisible() {

    return (
      this.branches.length > 1
    );
  }

  get selectedAccount() {

    return this.fundingAccounts.find(
      x =>
        x.id ===
        this.selectedAccountId
    );
  }
}