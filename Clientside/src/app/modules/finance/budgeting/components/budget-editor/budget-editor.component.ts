import { Component, OnInit, OnDestroy } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { MatSelectModule } from '@angular/material/select';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { Subscription } from 'rxjs';

import { BudgetService } from '../../services/budget.service';
import { AccountsService } from '../../../../accounts/services/accounts.service';
import { BranchService } from '../../../../branches/services/branch.service';
import { BranchContextService } from '../../../../../core/services/branch-context.service';
import { AuthService } from '../../../../auth/services/auth.service';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';

@Component({
  standalone: true,
  selector: 'app-budget-editor',
  imports: [
    CommonModule,
    FormsModule,
    MatSelectModule,
    MatSnackBarModule,
    MatFormFieldModule,
    MatInputModule
  ],
  templateUrl: './budget-editor.component.html',
  styleUrls: ['./budget-editor.component.scss']
})
export class BudgetEditorComponent implements OnInit, OnDestroy {

  fiscalYear = new Date().getFullYear();
  selectedBranchId: string | null = null;

  months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'];

  accounts: any[] = [];
  branches: any[] = [];

  budgetId!: string;
  data: Record<string, number[]> = {};

  private branchSub!: Subscription;

  constructor(
    private budgetService: BudgetService,
    private accountsService: AccountsService,
    private branchService: BranchService,
    private branchContext: BranchContextService,
    private snackbar: MatSnackBar,
    public auth: AuthService
  ) {}

  ngOnInit(): void {

    this.accountsService.list().subscribe(a => {
      this.accounts = a;
      this.initializeGrid();
    });

    this.branchService.getAll(false).subscribe(b => {
      this.branches = b;
    });

    // 🔥 Subscribe to branch context
    this.branchSub = this.branchContext.branch$.subscribe(branchId => {
      this.selectedBranchId = branchId;
      this.loadBudget();
    });

    this.loadBudget();
  }

  ngOnDestroy(): void {
    this.branchSub?.unsubscribe();
  }

  changeBranch(branchId: string | null) {
    this.branchContext.setBranch(branchId);
  }

  initializeGrid() {
    this.accounts.forEach(a => {
      this.data[a.id] = new Array(12).fill(0);
    });
  }

  loadBudget() {
    this.budgetService.get(this.selectedBranchId, this.fiscalYear)
      .subscribe({
        next: b => {
          if (!b) {
            this.createBudget();
            return;
          }

          this.budgetId = b.id;

          b.lines?.forEach((line: any) => {
            this.data[line.accountId] = line.months;
          });
        },
        error: () => this.createBudget()
      });
  }

  createBudget() {
    this.budgetService.create(this.selectedBranchId, this.fiscalYear)
      .subscribe(b => {
        this.budgetId = b.id;
      });
  }

  save(accountId: string, monthIndex: number) {
    const amount = this.data[accountId][monthIndex];

    this.budgetService.updateLine(
      this.budgetId,
      accountId,
      monthIndex + 1,
      amount
    ).subscribe({
      next: () =>
        this.snackbar.open('Saved', 'Close', { duration: 800 })
    });
  }
}