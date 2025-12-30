import { Component, Input, OnChanges, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ApexChartComponent } from '../../apex/apex-chart.component';

@Component({
  selector: 'app-donut-chart',
  standalone: true,
  imports: [CommonModule, ApexChartComponent],
  template: `
    <app-apex-chart
      [enabled]="labels.length > 0"
      [options]="options">
    </app-apex-chart>
  `
})
export class DonutChartComponent {

  @Input() labels: string[] = [];
  @Input() values: number[] = [];

  get options() {
    return {
      chart: { type: 'donut' },
      series: this.values,
      labels: this.labels,
      legend: { position: 'bottom' }
    };
  }
}