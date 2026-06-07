import {
    ChangeDetectionStrategy,
    Component,
    EventEmitter,
    Input,
    OnInit,
    Output,
    inject
} from '@angular/core';
import { CommonModule } from '@angular/common';
import {
    FormsModule
} from '@angular/forms';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatCheckboxModule
} from '@angular/material/checkbox';

import {
    MatFormFieldModule
} from '@angular/material/form-field';

import {
    MatInputModule
} from '@angular/material/input';

import {
    MatSelectModule
} from '@angular/material/select';
import { AccountsService } from '../../../../finance/accounts/services/accounts.service';
import { ApPaymentService } from '../../../../finance/ap/debts/services/ap-payment.service';
import { Account } from '../../../../finance/accounts/models/account.models';
import { FundingAccount } from '../../../../finance/ap/debts/models/funding-account.model';
import { SupplierService } from '../../../../suppliers/services/supplier.service';
import { SupplierMinimalDTO } from '../../../../suppliers/models/supplier.model';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';
import { OnboardingOperationalExpense } from '../../models/onboarding.models';

@Component({
    selector:
        'app-onboarding-review-step',
    standalone: true,
    imports: [
        CommonModule,
        FormsModule,
        MatButtonModule,
        MatCheckboxModule,
        MatFormFieldModule,
        MatInputModule,
        MatSelectModule,
        MatDatepickerModule,
        MatNativeDateModule
    ],
    templateUrl:
        './onboarding-review-step.component.html',
    styleUrls: [
        './onboarding-review-step.component.scss'
    ],
    changeDetection:
        ChangeDetectionStrategy.OnPush
})
export class OnboardingReviewStepComponent
    implements OnInit {

    private readonly accountsService =
        inject(AccountsService);

    private readonly paymentService =
        inject(ApPaymentService);

    private readonly supplierService =
        inject(SupplierService);

    suppliers:
        SupplierMinimalDTO[] = [];

    @Input({ required: true })
    state!: any;

    @Output()
    stateChange =
        new EventEmitter<any>();

    expenseAccounts:
        Account[] = [];

    fundingAccounts:
        FundingAccount[] = [];

    ngOnInit() {

        this.loadExpenseAccounts();
        this.loadFundingAccounts();
        this.loadSuppliers();
    }

    get selectedFundingAccount():
        FundingAccount | null {

        return (
            this.fundingAccounts.find(
                account =>
                    account.id ===
                    this.state
                        ?.fundingAccountId
            ) || null
        );
    }

    getReadonlyExpenseAccountLabel(): string {

        if (!this.expenseAccounts.length) {
            return '';
        }

        const account =
            this.expenseAccounts[0];

        return `${account.code} - ${account.name} (${account.balance.toFixed(2)})`;
    }

    getDisplayedExpenseAccount(
        expenseAccountId: string
    ): Account | undefined {

        if (
            this.expenseAccounts.length === 1
        ) {
            return this.expenseAccounts[0];
        }

        return this.getExpenseAccount(
            expenseAccountId
        );
    }

    get supplierFundingRequired(): number {

        return this.state.autoPaySuppliers
            ? this.totalInventoryValue
            : 0;
    }

    get expenseFundingRequired(): number {

        return this.state.autoPayOperationalExpenses
            ? this.totalOperationalExpenses
            : 0;
    }

    get totalUnits(): number {

        return (
            this.state?.suppliers || []
        ).reduce(
            (
                sum: number,
                row: any
            ) =>
                sum +
                Number(
                    row.unitsSupplied || 0
                ),
            0
        );
    }

    get totalInventoryValue(): number {

        return (
            this.state?.suppliers || []
        ).reduce(
            (
                sum: number,
                row: any
            ) =>
                sum +
                (
                    Number(
                        row.unitsSupplied || 0
                    ) *
                    Number(
                        row.unitCost || 0
                    )
                ),
            0
        );
    }

    get totalOperationalExpenses(): number {

        return (
            this.state?.operationalExpenses || []
        ).reduce(
            (
                sum: number,
                expense: any
            ) =>
                sum +
                Number(
                    expense.amount || 0
                ),
            0
        );
    }

    get totalFundingRequired(): number {

        return (
            this.supplierFundingRequired +
            this.expenseFundingRequired
        );
    }

    get fundingShortfall(): number {

        const balance =
            this.selectedFundingAccount
                ?.balance ?? 0;

        return Math.max(
            0,
            this.totalFundingRequired - balance
        );

    }

    get fundingSurplus(): number {

        const balance =
            this.selectedFundingAccount
                ?.balance ?? 0;

        return Math.max(
            0,
            balance - this.totalFundingRequired
        );

    }

    get requiresAutoFunding(): boolean {

        return this.fundingShortfall > 0;

    }

    getSupplierName(
        supplier: any
    ): string {

        if (
            supplier.supplierName
        ) {
            return supplier.supplierName;
        }

        return (
            this.suppliers.find(
                s =>
                    s.id ===
                    supplier.supplierId
            )?.name
            ||
            'Unknown Supplier'
        );
    }

    addExpense() {

        const defaultAccountId =
            this.expenseAccounts.length === 1
                ? this.expenseAccounts[0].id
                : '';

        this.state = {

            ...this.state,

            operationalExpenses: [

                ...(this.state.operationalExpenses || []),

                {
                    expenseAccountId:
                        defaultAccountId,
                    description: '',
                    amount: 0
                }

            ]
        };

        this.emit();
    }

    removeExpense(
        index: number
    ) {

        this.state = {

            ...this.state,

            operationalExpenses:
                this.state
                    .operationalExpenses
                    .filter(
                        (
                            _: any,
                            i: number
                        ) =>
                            i !== index
                    )
        };

        this.emit();
    }

    emit() {

        this.stateChange.emit(
            structuredClone(
                this.state
            )
        );
    }

    getPackagingName(
        packagingTempId: string
    ): string {

        const packaging =
            this.state?.packagings
                ?.find(
                    (p: any) =>
                        p.tempId ===
                        packagingTempId
                );

        return (
            packaging?.name ||
            'Unknown'
        );
    }

    private loadExpenseAccounts() {

        this.accountsService
            .list({
                page: 0,
                size: 1000
            })
            .subscribe({
                next: result => {

                    this.expenseAccounts =
                        result.content
                            .filter(
                                account =>
                                    account.type === 'EXPENSE'
                                    &&
                                    account.role !== 'COGS'
                                    &&
                                    account.role !== 'CORPORATE_TAX_EXPENSE'
                            );
                    if (
                        this.expenseAccounts.length === 1
                    ) {

                        const account =
                            this.expenseAccounts[0];

                        this.state = {

                            ...this.state,

                            operationalExpenses:
                                (
                                    this.state
                                        .operationalExpenses || []
                                ).map((expense: OnboardingOperationalExpense) => ({
                                    ...expense,
                                    expenseAccountId: account.id
                                }))
                        };

                        this.emit();
                    }
                }
            });
    }

    private loadFundingAccounts() {

        this.paymentService
            .fundingAccounts()
            .subscribe({
                next: accounts => {

                    this.fundingAccounts =
                        accounts;
                }
            });
    }

    private loadSuppliers() {

        this.supplierService
            .getAll(false)
            .subscribe({
                next: suppliers => {

                    this.suppliers =
                        suppliers.map(s => ({
                            id: s.id,
                            name: s.name
                        }));
                }
            });

    }

    getExpenseAccount(
        id: string
    ): Account | undefined {

        return this.expenseAccounts
            .find(
                account =>
                    account.id === id
            );
    }
}