import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';
import { MatIconModule } from '@angular/material/icon';

import { ReportsMetadataService } from '../../services/reports-metadata.service';
import { ReportMetadata } from '../../models/report-metadata.model';
import { ReportDialogComponent } from '../../components/report-dialog/report-dialog.component';
import { MatFormFieldModule } from '@angular/material/form-field';
import { FormsModule } from '@angular/forms';
import { MatInputModule } from '@angular/material/input';

@Component({
  selector: 'app-reports-dashboard',
  standalone: true,
  templateUrl: './reports-dashboard.component.html',
  styleUrls: ['./reports-dashboard.component.scss'],
  imports: [
    CommonModule,
    MatDialogModule,
    MatIconModule,
    MatFormFieldModule,
    MatInputModule,
    FormsModule
  ]
})
export class ReportsDashboardComponent implements OnInit {

  groupedReports: Record<string, ReportMetadata[]> = {};

  searchTerm = '';
  mostUsed: ReportMetadata[] = [];

  constructor(
    private reportsMeta: ReportsMetadataService,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {
    this.reportsMeta.list().subscribe(reports => {

      this.groupedReports = reports.reduce((acc, r) => {
        acc[r.category] ??= [];
        acc[r.category].push(r);
        return acc;
      }, {} as Record<string, ReportMetadata[]>);

      this.loadMostUsed(reports);
    });
  }

  get filteredGroups(): Record<string, ReportMetadata[]> {

    if (!this.searchTerm?.trim()) {
      return this.groupedReports;
    }

    const term = this.searchTerm.toLowerCase();

    const filtered: Record<string, ReportMetadata[]> = {};

    for (const category of Object.keys(this.groupedReports)) {

      const matches = this.groupedReports[category].filter(r =>
        r.title.toLowerCase().includes(term) ||
        r.description.toLowerCase().includes(term)
      );

      if (matches.length) {
        filtered[category] = matches;
      }
    }

    return filtered;
  }

  get showMostUsed(): boolean {
    return !this.searchTerm?.trim() && this.mostUsed.length > 0;
  }

  private recordUsage(key: string) {

    const usage = JSON.parse(localStorage.getItem('reportUsage') || '{}');

    usage[key] = (usage[key] || 0) + 1;

    localStorage.setItem('reportUsage', JSON.stringify(usage));

    // 🔥 Immediate UI refresh
    const allReports = Object.values(this.groupedReports).flat();
    this.loadMostUsed(allReports);
  }

  private loadMostUsed(all: ReportMetadata[]) {

    const usage = JSON.parse(localStorage.getItem('reportUsage') || '{}');

    this.mostUsed = [...all]
      .filter(r => usage[r.key]) // 🔥 only show used ones
      .sort((a, b) => (usage[b.key] || 0) - (usage[a.key] || 0))
      .slice(0, 10);
  }

  open(report: ReportMetadata) {

    this.recordUsage(report.key);

    this.dialog.open(ReportDialogComponent, {
      width: '420px',
      maxWidth: '95vw',
      autoFocus: false,
      data: report
    });
  }
}