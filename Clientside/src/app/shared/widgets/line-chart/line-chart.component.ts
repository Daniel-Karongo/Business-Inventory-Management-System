import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ApexChartComponent } from '../../apex/apex-chart.component';

@Component({
  selector: 'app-line-chart',
  standalone: true,
  imports: [CommonModule, ApexChartComponent],
  template: `<app-apex-chart [options]="chartOptions" class="w-full h-full"></app-apex-chart>`,
  styles: [':host { display:block; height:100%; }']
})
export class LineChartComponent {
  chartOptions: any = {
    series: [
      { name: 'Revenue', data: [350, 420, 480, 600, 720, 680, 790, 860, 930, 1000, 1200, 1400] }
    ],
    chart: {
      height: 350,
      type: 'line',
      toolbar: { show: false }
    },
    xaxis: {
      categories: ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
    },
    stroke: { curve: 'smooth', width: 3 },
    dataLabels: { enabled: false },
    tooltip: { enabled: true },
    grid: { show: false },
    fill: { opacity: 0.12 },
    colors: ['#0ea5e9'] // tailwind sky-500 / adjust as needed
  };
}