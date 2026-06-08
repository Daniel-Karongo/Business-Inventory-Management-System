import { CommonModule } from '@angular/common';
import { Component, Inject, inject } from '@angular/core';
import { FormBuilder, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatButtonModule } from '@angular/material/button';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MAT_DIALOG_DATA, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { finalize } from 'rxjs';

import { FundingAccount } from '../../models/funding-account.model';
import { ApPaymentService } from '../../services/ap-payment.service';

@Component({
  selector: 'app-create-payment-dialog',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatDialogModule,
    MatButtonModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatCheckboxModule,
    MatDatepickerModule,
    MatIconModule,
    MatSnackBarModule
  ],
  templateUrl: './create-payment-dialog.component.html',
  styleUrls: ['./create-payment-dialog.component.scss']
})
export class CreatePaymentDialogComponent {

  private fb = inject(FormBuilder);
  private payments = inject(ApPaymentService);
  private snack = inject(MatSnackBar);

  loading = false;

  accounts: FundingAccount[] = [];
  get selectedAccount(): FundingAccount | undefined {

    const id =
      this.form.get('fundingAccountId')?.value;

    return this.accounts.find(
      a => a.id === id
    );
  }

  get derivedMethod(): string {

    return this.selectedAccount?.role || '-';
  }

  get selectedBalance(): number {

    return this.selectedAccount?.balance || 0;
  }

  get insufficientFunds(): boolean {

    return Number(
      this.form.get('amount')?.value || 0
    ) > this.selectedBalance;
  }

  readonly form = this.fb.nonNullable.group({
    fundingAccountId: ['', Validators.required],
    amount: [0, [Validators.required, Validators.min(0.01)]],
    reference: [''],
    paymentDate: [new Date(), Validators.required],
    autoPost: [true],
    autoAllocate: [true]
  });

  constructor(
    @Inject(MAT_DIALOG_DATA)
    public data: {
      branchId: string;
      supplierId: string;
    },
    private ref: MatDialogRef<CreatePaymentDialogComponent>
  ) { }

  ngOnInit(): void {
    this.loading = true;

    this.payments
      .fundingAccounts()
      .pipe(finalize(() => this.loading = false))
      .subscribe({
        next: res => this.accounts = res,
        error: err => this.error(err)
      });
  }

  submit(): void {

    if (
      this.loading
      || this.form.invalid
    ) {
      return;
    }

    if (!this.selectedAccount) {
      return;
    }

    this.loading = true;

    const v =
      this.form.getRawValue();

    this.payments.process({

      branchId:
        this.data.branchId,

      supplierId:
        this.data.supplierId,

      fundingAccountId:
        v.fundingAccountId,

      amount:
        Number(v.amount),

      method:
        this.selectedAccount.role as any,

      reference:
        v.reference || undefined,

      paymentDate:
        new Date(v.paymentDate)
          .toISOString()
          .split('T')[0],

      autoPost:
        v.autoPost,

      autoAllocate:
        v.autoAllocate

    })
      .pipe(
        finalize(() =>
          this.loading = false
        )
      )
      .subscribe({

        next: () => {

          this.snack.open(
            'Payment processed successfully',
            'Close',
            {
              duration: 3000
            }
          );

          this.ref.close(true);
        },

        error: err =>
          this.error(err)
      });
  }

  private error(err: any): void {
    this.snack.open(
      err?.message || 'Operation failed',
      'Close',
      { duration: 5000 }
    );
  }
}