import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatSelectModule } from '@angular/material/select';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';

import { BudgetService } from '../../services/budget.service';
import { AccountsService } from '../../../../accounts/services/accounts.service';
import { BranchContextService } from '../../../../../core/services/branch-context.service';

@Component({
  standalone: true,
  selector: 'app-budget-variance',
  imports: [
    CommonModule,
    FormsModule,
    MatFormFieldModule,
    MatSelectModule,
    MatSlideToggleModule
  ],
  templateUrl: './budget-variance.component.html',
  styleUrls: ['./budget-variance.component.scss']
})
export class BudgetVarianceComponent implements OnInit {

  year = new Date().getFullYear();
  month = new Date().getMonth() + 1;

  accountId!: string;

  consolidated = false;

  loading = false;
  result: any = null;

  accounts: any[] = [];
  branchId: string | null = null;

  constructor(
    private budgetService: BudgetService,
    private accountsService: AccountsService,
    private branchContext: BranchContextService
  ) { }

  ngOnInit(): void {

    this.accountsService.list().subscribe(a => this.accounts = a);

    this.branchContext.branch$.subscribe(b => {
      this.branchId = b;
      this.load();
    });
  }

  load() {

    if (!this.accountId) return;

    this.loading = true;

    if (this.consolidated) {

      this.budgetService.corporateVariance(
        this.year,
        this.month,
        this.accountId
      ).subscribe(res => {
        this.result = res;
        this.loading = false;
      });

    } else {

      if (!this.branchId) {
        this.loading = false;
        this.result = null;
        return;
      }

      this.budgetService.branchVariance(
        this.branchId,
        this.year,
        this.month,
        this.accountId
      ).subscribe(res => {
        this.result = res;
        this.loading = false;
      });

    }
  }
}