import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';
import { MatIconModule } from '@angular/material/icon';

import { ReportsMetadataService } from '../../services/reports-metadata.service';
import { ReportMetadata } from '../../models/report-metadata.model';
import { ReportDialogComponent } from '../../components/report-dialog/report-dialog.component';

@Component({
  selector: 'app-reports-dashboard',
  standalone: true,
  templateUrl: './reports-dashboard.component.html',
  styleUrls: ['./reports-dashboard.component.scss'],
  imports: [
    CommonModule,
    MatDialogModule,
    MatIconModule
  ]
})
export class ReportsDashboardComponent implements OnInit {

  groupedReports: Record<string, ReportMetadata[]> = {};

  constructor(
    private reportsMeta: ReportsMetadataService,
    private dialog: MatDialog
  ) {}

  ngOnInit(): void {
    this.reportsMeta.list().subscribe(reports => {
      this.groupedReports = reports.reduce((acc, r) => {
        acc[r.category] ??= [];
        acc[r.category].push(r);
        return acc;
      }, {} as Record<string, ReportMetadata[]>);
    });
  }

  open(report: ReportMetadata) {
    this.dialog.open(ReportDialogComponent, {
      width: '420px',
      maxWidth: '95vw',
      autoFocus: false,
      data: report
    });
  }
}