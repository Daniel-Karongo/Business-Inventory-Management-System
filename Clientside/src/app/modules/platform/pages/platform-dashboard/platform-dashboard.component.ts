import { Component, inject, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';

import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';

import { PlatformMetricsService } from '../../services/platform-metrics.service';
import { PlatformAuditService, AuditLog } from '../../services/platform-audit.service';
import { MatDialog } from '@angular/material/dialog';
import { BiometricPromptDialog } from '../../../../shared/components/biometric-prompt-dialog/biometric-prompt-dialog.component';
import { BiometricRegistrationService } from '../../../../core/services/biometric-registration.service';

@Component({
  standalone: true,
  selector: 'app-platform-dashboard',
  templateUrl: './platform-dashboard.component.html',
  styleUrls: ['./platform-dashboard.component.scss'],
  imports: [
    CommonModule,
    RouterModule,
    MatCardModule,
    MatIconModule,
    MatButtonModule
  ]
})
export class PlatformDashboardComponent implements OnInit {

  private metrics = inject(PlatformMetricsService);
  private audit = inject(PlatformAuditService);
  private dialog = inject(MatDialog);
  private biometric = inject(BiometricRegistrationService);

  totalRequests = 0;

  totalErrors = 0;

  auditLogs: AuditLog[] = [];

  ngOnInit() {

    this.loadMetrics();

    this.loadAudit();

  }

  loadMetrics() {

    this.metrics.getTotalRequests()
      .subscribe(v => this.totalRequests = v);

    this.metrics.getTotalErrors()
      .subscribe(v => this.totalErrors = v);

  }

  loadAudit() {

    this.audit.getAudit(0, 5)
      .subscribe(page => {

        this.auditLogs = page.content ?? [];

      });

  }

  debugRegister() {
    const ref = this.dialog.open(BiometricPromptDialog);

    ref.afterClosed().subscribe((enable) => {
      if (enable === true) {
        this.biometric.register();
      }
    });
  }

}