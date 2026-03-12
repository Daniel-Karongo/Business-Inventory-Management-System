import { Component, Inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MAT_DIALOG_DATA, MatDialogModule } from '@angular/material/dialog';

export interface FinancialDetailData {
  type: 'AR_AGING' | 'AP_AGING' | 'BUDGET';
  title: string;
  payload: any;
}

@Component({
  selector: 'app-financial-detail-dialog',
  standalone: true,
  imports: [CommonModule, MatDialogModule],
  templateUrl: './financial-detail-dialog.component.html',
  styleUrls: ['./financial-detail-dialog.component.scss']
})
export class FinancialDetailDialogComponent {

  constructor(
    @Inject(MAT_DIALOG_DATA)
    public data: FinancialDetailData
  ) {}

  get agingBuckets() {
    if (!this.data?.payload) return [];

    return [
      { label: 'Current', value: this.data.payload.current },
      { label: '0-30 Days', value: this.data.payload.days30 },
      { label: '31-60 Days', value: this.data.payload.days60 },
      { label: '61-90 Days', value: this.data.payload.days90 },
      { label: '90+ Days', value: this.data.payload.over90 }
    ];
  }
}