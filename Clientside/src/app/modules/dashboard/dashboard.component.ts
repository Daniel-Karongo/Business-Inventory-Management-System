import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { StatCardComponent } from '../../shared/widgets/stat-card/stat-card.component';
import { BarChartComponent } from '../../shared/widgets/bar-chart/bar-chart.component';
import { ActivityListComponent } from '../../shared/widgets/activity-list/activity-list.component';
import { DonutChartComponent } from '../../shared/widgets/donut-chart/donut-chart.component';

@Component({
  selector: 'app-dashboard',
  standalone: true,
  imports: [CommonModule, StatCardComponent, DonutChartComponent, BarChartComponent, ActivityListComponent],
  templateUrl: './dashboard.component.html',
  styleUrls: ['./dashboard.component.scss']
})
export class DashboardComponent {
  stats = [
    { title: 'Sales Today', value: 'KSh 128,450', change: '+12%' },
    { title: 'Inventory Value', value: 'KSh 6,420,900', change: '-2%' },
    { title: 'Profit', value: 'KSh 42,300', change: '+5%' },
    { title: 'Customers', value: '1,248', change: '+1.2%' },
  ];
}