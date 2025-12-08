import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';

interface Activity {
  id: string;
  type: string;
  description: string;
  time: string;
  actor?: string;
}

@Component({
  selector: 'app-activity-list',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './activity-list.component.html',
  styleUrls: ['./activity-list.component.scss']
})
export class ActivityListComponent {
  activities: Activity[] = [
    { id: '1', type: 'Sale', description: 'Sold 3 units of iPhone 15', time: '5m ago', actor: 'Isaac' },
    { id: '2', type: 'Stock', description: 'Received 50 units of Charger', time: '12m ago', actor: 'Warehouse' },
    { id: '3', type: 'User', description: 'New user created: alice@example.com', time: '1h ago', actor: 'System' },
    { id: '4', type: 'Refund', description: 'Refund issued for Invoice #345', time: '3h ago', actor: 'Finance' },
  ];
}