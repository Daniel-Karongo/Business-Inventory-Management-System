import { Component, inject, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ActivatedRoute } from '@angular/router';

import { PlanService } from '../../../plans/services/plan.service';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import { TenantSubscriptionService } from '../../services/tenant-subscription.service';
import { PlanResponse } from '../../../plans/models/plan.model';

@Component({
  standalone: true,
  selector: 'app-tenant-subscription',
  templateUrl: './tenant-subscription.component.html',
  styleUrls: ['./tenant-subscription.component.scss'],
  imports: [
    CommonModule,
    MatFormFieldModule,
    MatSelectModule,
    MatButtonModule,
    MatSnackBarModule
  ]
})
export class TenantSubscriptionComponent implements OnInit {

  private route = inject(ActivatedRoute);

  private planService = inject(PlanService);

  private subscriptionService = inject(TenantSubscriptionService);

  private snack = inject(MatSnackBar);

  tenantId!: string;

  plans: PlanResponse[] = [];

  selectedPlan?: string;

  ngOnInit() {

    this.tenantId = this.route.snapshot.paramMap.get('id')!;

    this.loadPlans();

  }

  loadPlans() {

    this.planService.getPlans()
      .subscribe(plans => this.plans = plans);

  }

  assignPlan() {

    if (!this.selectedPlan) return;

    this.subscriptionService
      .assignPlan(this.tenantId, this.selectedPlan)
      .subscribe({

        next: () => {

          this.snack.open(
            'Plan updated successfully',
            'Close',
            { duration: 3000 }
          );

        }

      });

  }

}