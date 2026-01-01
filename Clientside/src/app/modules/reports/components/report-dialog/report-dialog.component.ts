import { Component, Inject, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogModule } from '@angular/material/dialog';
import { ReportsService } from '../../services/reports.service';
import { AccountsService } from '../../../accounts/services/accounts.service';
import { ReportDefinition } from '../../models/report.model';
import { MatFormFieldModule } from '@angular/material/form-field';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';

@Component({
  templateUrl: './report-dialog.component.html',
  styleUrls: ['./report-dialog.component.scss'],
  imports: [
    CommonModule,
    FormsModule,
    MatDialogModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatButtonModule,
    MatDatepickerModule,
    MatNativeDateModule
  ]
})
export class ReportDialogComponent implements OnInit {

  from!: Date;
  to!: Date;
  accountId?: string;

  accounts: any[] = [];
  pdfUrl?: string;
  loading = false;

  constructor(
    @Inject(MAT_DIALOG_DATA) public report: ReportDefinition,
    private reports: ReportsService,
    private accountsSvc: AccountsService
  ) { }

  ngOnInit() {
    const start = new Date(2025, 0, 1);   // Jan 1, 2025
    const end = new Date(2026, 11, 31); // Dec 31, 2026

    this.from = start;
    this.to = end;

    if (this.report.requiresAccount) {
      this.accountsSvc.list().subscribe(a => this.accounts = a);
    }
  }

  private toIso(date: Date): string {
    return date.toISOString().slice(0, 10);
  }

  generate() {
    this.loading = true;

    const params: any = {
      FROM_DATE: this.toIso(this.from),
      TO_DATE: this.toIso(this.to)
    };

    if (this.report.requiresAccount) {
      params.ACCOUNT_ID = this.accountId;
    }

    this.reports.generate(this.report.key, params).subscribe(blob => {
      const url = URL.createObjectURL(blob);
      this.pdfUrl = url;
      window.open(url, '_blank'); // auto-open
      this.loading = false;
    });
  }

  openInNewTab() {
    if (!this.pdfUrl) return;
    window.open(this.pdfUrl, '_blank', 'noopener');
  }
}