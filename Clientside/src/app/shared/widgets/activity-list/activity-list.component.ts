import { Component, Input } from '@angular/core';
import { CommonModule } from '@angular/common';
import { DateUtilsService } from '../../../core/services/date-utils';

export interface Activity {
  type: string;        // SALE | STOCK | USER | PRODUCT | SUPPLIER
  description: string;
  actor?: string;
  time: string;
}

@Component({
  selector: 'app-activity-list',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './activity-list.component.html',
  styleUrls: ['./activity-list.component.scss']
})
export class ActivityListComponent {
  @Input() activities: Activity[] = [];

  constructor(public dateUtils: DateUtilsService) { }

  badgeClass(type: string): string {
    switch (type) {
      case 'SALE': return 'badge-sale';
      case 'STOCK': return 'badge-stock';
      case 'USER': return 'badge-user';
      case 'PRODUCT': return 'badge-product';
      case 'SUPPLIER': return 'badge-supplier';
      default: return 'badge-default';
    }
  }
}