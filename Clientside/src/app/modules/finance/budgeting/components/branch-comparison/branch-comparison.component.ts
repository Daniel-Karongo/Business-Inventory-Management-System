import { Component, OnInit, OnDestroy } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Subscription } from 'rxjs';

import { BudgetService } from '../../services/budget.service';
import { BranchContextService } from '../../../../../core/services/branch-context.service';

@Component({
  standalone: true,
  selector: 'app-branch-comparison',
  imports: [CommonModule],
  templateUrl: './branch-comparison.component.html',
  styleUrls: ['./branch-comparison.component.scss']
})
export class BranchComparisonComponent implements OnInit, OnDestroy {

  year = new Date().getFullYear();
  month = new Date().getMonth() + 1;
  accountId = '';

  data: any[] = [];

  private branchSub!: Subscription;
  private activeBranch: string | null = null;

  constructor(
    private budgetService: BudgetService,
    private branchContext: BranchContextService
  ) {}

  ngOnInit(): void {
    this.branchSub = this.branchContext.branch$.subscribe(branchId => {
      this.activeBranch = branchId;
      this.load();
    });
  }

  ngOnDestroy(): void {
    this.branchSub?.unsubscribe();
  }

  load() {
    this.budgetService.compareBranches(
      this.year,
      this.month,
      this.accountId
    ).subscribe(res => {
      this.data = res as any[];
    });
  }
}