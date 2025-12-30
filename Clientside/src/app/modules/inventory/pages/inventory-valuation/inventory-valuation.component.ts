import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatSelectModule } from '@angular/material/select';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';

import { InventoryValuationService, CostBasis } from '../../services/inventory-valuation.service';

import { forkJoin } from 'rxjs';
import { BarChartComponent } from '../../../../shared/widgets/bar-chart/bar-chart.component';

@Component({
  standalone: true,
  selector: 'app-inventory-valuation',
  imports: [
    CommonModule,
    MatFormFieldModule,
    MatSelectModule,
    MatProgressSpinnerModule,
    BarChartComponent
  ],
  templateUrl: './inventory-valuation.component.html',
  styleUrls: ['./inventory-valuation.component.scss']
})
export class InventoryValuationComponent implements OnInit {

  loading = true;

  totalValue = 0;
  valuationMethod = 'WAC';

  categoryLabels: string[] = [];
  categoryValues: number[] = [];

  constructor(
    private valuationService: InventoryValuationService
  ) {}

  ngOnInit(): void {
    this.load();
  }

  load(): void {
    this.loading = true;

    forkJoin({
      total: this.valuationService.getTotal(),
      categories: this.valuationService.getCategoryValuation()
    }).subscribe({
      next: ({ total, categories }) => {
        this.totalValue = total.totalValuation;
        this.valuationMethod = total.valuationMethod;

        this.categoryLabels = Object.keys(categories);
        this.categoryValues = Object.values(categories);
      },
      complete: () => (this.loading = false)
    });
  }
}