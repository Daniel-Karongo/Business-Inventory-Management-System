import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ApexChartComponent } from '../../apex/apex-chart.component';

@Component({
  selector: 'app-bar-chart',
  standalone: true,
  imports: [CommonModule, ApexChartComponent],
  template: `<app-apex-chart [options]="options" class="w-full h-full"></app-apex-chart>`,
  styles: [':host { display:block; height:100%; }']
})
export class BarChartComponent {
  options: any = {
    series: [
      { name: 'Electronics', data: [44, 55, 41, 67, 22] },
      { name: 'Accessories', data: [13, 23, 20, 8, 13] }
    ],
    chart: { type: 'bar', height: 300, toolbar: { show: false } },
    plotOptions: { bar: { borderRadius: 6, columnWidth: '45%' } },
    xaxis: { categories: ['Phones', 'Laptops', 'Headphones', 'Chargers', 'Cables'] },
    dataLabels: { enabled: false },
    colors: ['#06b6d4','#60a5fa'], // teal-400, blue-400
    legend: { show: true }
  };
}