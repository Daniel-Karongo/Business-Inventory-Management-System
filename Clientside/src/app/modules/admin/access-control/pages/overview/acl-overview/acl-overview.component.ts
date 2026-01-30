import { Component, OnInit, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatButtonModule } from '@angular/material/button';
import { MatTableModule } from '@angular/material/table';
import { MatIconModule } from '@angular/material/icon';
import { MatTooltipModule } from '@angular/material/tooltip';

import { AclOverviewService } from './acl-overview.service';

@Component({
  selector: 'app-acl-overview',
  standalone: true,
  imports: [
    CommonModule,
    MatButtonModule,
    MatTableModule,
    MatIconModule,
    MatTooltipModule
  ],
  templateUrl: './acl-overview.component.html',
  styleUrls: ['./acl-overview.component.scss']
})
export class AclOverviewComponent implements OnInit {

  private acl = inject(AclOverviewService);

  summary: any;
  audits: any[] = [];

  roleColumns = ['role', 'count'];
  auditColumns = ['time', 'entity', 'action', 'actor'];

  private readonly ROLE_ORDER = [
    'SUPERUSER',
    'ADMIN',
    'MANAGER',
    'SUPERVISOR',
    'EMPLOYEE'
  ];
  
  ngOnInit(): void {
    this.load();
  }

  load() {
    this.acl.getSummary().subscribe(s => this.summary = s);
    this.acl.getRecentAudits().subscribe(a => this.audits = a);
  }

  refreshCache() {
    this.acl.refreshCache().subscribe(() => this.load());
  }

  roleEntries() {
    const entries = Object.entries(this.summary?.rolePermissionCounts ?? {})
      .map(([role, count]) => ({ role, count }));

    return entries.sort(
      (a, b) =>
        this.ROLE_ORDER.indexOf(a.role) -
        this.ROLE_ORDER.indexOf(b.role)
    );
  }
}