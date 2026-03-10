import { Component, inject, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';

import { PlanService } from '../../services/plan.service';
import { PlanResponse } from '../../models/plan.model';

import { MatTableModule } from '@angular/material/table';
import { MatButtonModule } from '@angular/material/button';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

@Component({
  standalone: true,
  selector: 'app-plan-list',
  templateUrl: './plan-list.component.html',
  styleUrls: ['./plan-list.component.scss'],
  imports: [
    CommonModule,
    MatTableModule,
    MatButtonModule,
    MatSnackBarModule
  ]
})
export class PlanListComponent implements OnInit {

  private planService = inject(PlanService);

  private router = inject(Router);

  private snack = inject(MatSnackBar);

  plans: PlanResponse[] = [];

  displayedColumns = [
    'code',
    'name',
    'maxUsers',
    'maxBranches',
    'actions'
  ];

  ngOnInit() {

    this.loadPlans();

  }

  loadPlans() {

    this.planService.getPlans().subscribe({

      next: (plans) => {

        this.plans = plans;

      },

      error: () => {

        this.snack.open(
          'Failed to load plans',
          'Close',
          { duration: 4000 }
        );

      }

    });

  }

  createPlan() {

    this.router.navigate(['/platform/plans/create']);

  }

  delete(id: string) {

    this.planService.deletePlan(id).subscribe({

      next: () => {

        this.snack.open(
          'Plan deleted',
          'Close',
          { duration: 2500 }
        );

        this.loadPlans();

      }

    });

  }

}