import { CommonModule } from '@angular/common';
import { Component, OnInit, inject } from '@angular/core';
import { FormsModule } from '@angular/forms';

import {
  DeviceApiService,
  DeviceDTO
} from '../../../../../core/services/device-api.service';

import { BranchContextService } from '../../../../../core/services/branch-context.service';

import { MatButtonModule } from '@angular/material/button';
import { MatButtonToggleModule } from '@angular/material/button-toggle';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatTableModule } from '@angular/material/table';

import { MatDialog } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatTooltipModule } from '@angular/material/tooltip';
import { forkJoin } from 'rxjs';
import { DeviceAuditDialogComponent } from '../../../../../shared/components/device-audit-dialog/device-audit-dialog.component';
import { RenameDeviceDialogComponent } from '../../../../../shared/components/rename-device-dialog/rename-device-dialog.component';
import { PageShellComponent } from '../../../../../shared/layout/page-shell/page-shell.component';

@Component({
  standalone: true,
  selector: 'app-tenant-devices',
  imports: [
    CommonModule,
    FormsModule,
    PageShellComponent,

    MatTableModule,
    MatPaginatorModule,
    MatButtonModule,
    MatInputModule,
    MatFormFieldModule,
    MatIconModule,
    MatTooltipModule,
    MatButtonToggleModule,
    MatCheckboxModule
  ],
  templateUrl: './tenant-devices.component.html',
  styleUrls: ['./tenant-devices.component.scss']
})
export class TenantDevicesComponent implements OnInit {

  private api = inject(DeviceApiService);
  private branchCtx = inject(BranchContextService);
  private dialog = inject(MatDialog);
  private snack = inject(MatSnackBar);

  loading = true;
  actionBusy = false;

  devices: DeviceDTO[] = [];
  filtered: DeviceDTO[] = [];
  paged: DeviceDTO[] = [];

  searchTerm = '';
  statusFilter = 'ALL';

  page = 0;
  size = 10;
  total = 0;

  displayedColumns = [
    'select',
    'device',
    'status',
    'usedBy',
    'lastSeen',
    'actions'
  ];

  stats: any = {
    approvedDevices: 0,
    pendingDevices: 0,
    rejectedDevices: 0,
    devicesInUse: 0
  };

  viewMode: 'table' | 'cards' = 'table';

  selectedIds = new Set<string>();

  mode: 'all' | 'pending' = 'all';
  compactView = false;
  hasSelection = false;

  ngOnInit() {

    const savedView =
      localStorage.getItem('devices.viewMode');

    const savedMode =
      localStorage.getItem('devices.mode');

    const compact =
      localStorage.getItem('devices.compact');

    if (
      savedView === 'table' ||
      savedView === 'cards'
    ) {
      this.viewMode = savedView;
    }

    if (
      savedMode === 'pending' ||
      savedMode === 'all'
    ) {
      this.mode = savedMode;
    }

    if (compact === 'true') {
      this.compactView = true;
      this.size = this.compactView ? 25 : 10;
    }

    this.load();
  }

  get allVisibleSelected(): boolean {
    return this.paged.length > 0 &&
      this.paged.every(d => this.selectedIds.has(d.id));
  }

  get partiallyVisibleSelected(): boolean {
    return this.selectedIds.size > 0 &&
      !this.allVisibleSelected;
  }

  toggleDensity() {

    this.compactView = !this.compactView;

    localStorage.setItem(
      'devices.compact',
      String(this.compactView)
    );

    this.size =
      this.compactView ? 25 : 10;

    this.applyPagination();

  }

  setView(
    mode: 'table' | 'cards'
  ) {
    this.viewMode = mode;

    localStorage.setItem(
      'devices.viewMode',
      mode
    );
  }

  setMode(
    mode: 'all' | 'pending'
  ) {
    this.mode = mode;

    localStorage.setItem(
      'devices.mode',
      mode
    );

    if (mode === 'pending') {
      this.loadPending();
    } else {
      this.load();
    }
  }

  load() {

    this.loading = true;

    this.api.stats().subscribe({
      next: s => this.stats = s
    });

    this.api.list(
      this.branchCtx.currentBranch
    ).subscribe({
      next: data => {
        this.devices = data;
        this.applySearch();
        this.loading = false;
      },
      error: () => this.loading = false
    });

  }

  applySearch() {

    const term =
      this.searchTerm
        .trim()
        .toLowerCase();

    this.filtered =
      this.devices.filter(d => {

        const deviceMatch =
          d.deviceName
            ?.toLowerCase()
            .includes(term);

        const userMatch =
          (d.usedByUsernames || [])
            .join(', ')
            .toLowerCase()
            .includes(term);

        const statusMatch =
          this.statusFilter === 'ALL'
          || d.status === this.statusFilter;

        return (
          (!term || deviceMatch || userMatch)
          && statusMatch
        );
      });

    this.page = 0;

    this.applyPagination();

  }

  applyPagination() {

    this.total = this.filtered.length;

    const start = this.page * this.size;

    this.paged =
      this.filtered.slice(
        start,
        start + this.size
      );

  }

  changePage(e: PageEvent) {

    this.page = e.pageIndex;
    this.size = e.pageSize;

    this.applyPagination();

  }

  approve(d: DeviceDTO) {

    this.api.approve(
      d.id,
      d.status === 'REJECTED'
        ? 'Recovered rejected device'
        : 'Approved by admin'
    ).subscribe(() => {

      this.snack.open(
        d.status === 'REJECTED'
          ? 'Device restored and approved'
          : 'Device approved',
        'Close',
        { duration: 3000 }
      );

      this.load();

    });

  }

  reject(d: DeviceDTO) {

    this.api.reject(
      d.id,
      'Revoked by admin'
    ).subscribe(() => {

      this.snack.open(
        'Device revoked',
        'Close',
        { duration: 2500 }
      );

      this.load();

    });

  }

  rename(d: DeviceDTO) {

    const ref = this.dialog.open(
      RenameDeviceDialogComponent,
      {
        width: '420px',
        data: {
          currentName: d.deviceName
        }
      }
    );

    ref.afterClosed()
      .subscribe(name => {

        if (!name?.trim()) return;

        this.api.rename(
          d.id,
          name.trim()
        ).subscribe(() => {

          this.snack.open(
            'Device renamed',
            'Close',
            { duration: 2500 }
          );

          this.load();

        });

      });

  }

  audit(d: DeviceDTO) {

    this.api.audit(
      d.id
    ).subscribe(rows => {

      this.dialog.open(
        DeviceAuditDialogComponent,
        {
          width: 'min(900px,95vw)',
          maxWidth: '95vw',
          data: rows
        }
      );

    });

  }

  loadPending() {

    this.loading = true;

    this.api.pending()
      .subscribe({
        next: data => {
          this.devices = data;
          this.applySearch();
          this.loading = false;
        },
        error: () => this.loading = false
      });

  }

  toggleSelected(id: string) {

    if (this.selectedIds.has(id)) {
      this.selectedIds.delete(id);
    } else {
      this.selectedIds.add(id);
    }

    this.selectedIds = new Set(this.selectedIds);

    this.hasSelection =
      this.selectedIds.size > 0;
  }

  toggleAll() {

    const visible = this.paged.map(x => x.id);

    const allSelected =
      visible.every(id =>
        this.selectedIds.has(id)
      );

    if (allSelected) {

      visible.forEach(id =>
        this.selectedIds.delete(id)
      );

    } else {

      visible.forEach(id =>
        this.selectedIds.add(id)
      );
    }

    this.selectedIds = new Set(this.selectedIds);

    this.hasSelection =
      this.selectedIds.size > 0;
  }

  bulkApprove() {

    const ids = [...this.selectedIds];

    if (!ids.length) {
      return;
    }

    this.actionBusy = true;

    forkJoin(
      ids.map(id =>
        this.api.approve(
          id,
          'Bulk approved'
        ))
    )
      .subscribe({

        next: () => {

          this.actionBusy = false;

          this.snack.open(
            'All selected devices approved',
            'Close',
            { duration: 3000 }
          );

          this.selectedIds.clear();
          this.hasSelection = false;

          this.load();

        },

        error: (err) => {

          this.actionBusy = false;

          const conflict =
            err?.error?.code ===
            'CONCURRENT_UPDATE';

          this.snack.open(
            conflict
              ? 'Some devices changed elsewhere. Reloaded.'
              : 'Bulk action failed',
            'Close',
            { duration: 4500 }
          );

          this.load();

        }

      });

  }

  bulkReject() {

    const ids = [...this.selectedIds];

    if (!ids.length) {
      return;
    }

    this.actionBusy = true;

    forkJoin(
      ids.map(id =>
        this.api.reject(
          id,
          'Bulk rejected'
        ))
    )
      .subscribe({

        next: () => {

          this.actionBusy = false;

          this.snack.open(
            'All selected devices rejected',
            'Close',
            { duration: 3000 }
          );

          this.selectedIds.clear();
          this.hasSelection = false;

          this.load();

        },

        error: (err) => {

          this.actionBusy = false;

          const conflict =
            err?.error?.code ===
            'CONCURRENT_UPDATE';

          this.snack.open(
            conflict
              ? 'Some devices changed elsewhere. Reloaded.'
              : 'Bulk action failed',
            'Close',
            { duration: 4500 }
          );

          this.load();

        }

      });

  }
}
