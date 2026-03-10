import { Component, inject, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';

import { PlatformAuditService, AuditLog } from '../../services/platform-audit.service';

import { MatTableModule } from '@angular/material/table';
import { MatPaginatorModule } from '@angular/material/paginator';
import { PageEvent } from '@angular/material/paginator';

@Component({
  standalone: true,
  selector: 'app-platform-audit',
  templateUrl: './platform-audit.component.html',
  styleUrls: ['./platform-audit.component.scss'],
  imports: [
    CommonModule,
    MatTableModule,
    MatPaginatorModule
  ]
})
export class PlatformAuditComponent implements OnInit {

  private auditService = inject(PlatformAuditService);

  logs: AuditLog[] = [];

  total = 0;

  page = 0;

  size = 20;

  displayedColumns = [
    'entityType',
    'action',
    'tenantId',
    'userId',
    'timestamp'
  ];

  ngOnInit() {

    this.load();

  }

  load() {

    this.auditService.getAudit(this.page, this.size)
      .subscribe(page => {

        this.logs = page.content ?? [];

        this.total = page.totalElements;

      });

  }

  changePage(event: PageEvent) {

    this.page = event.pageIndex;

    this.size = event.pageSize;

    this.load();

  }

}