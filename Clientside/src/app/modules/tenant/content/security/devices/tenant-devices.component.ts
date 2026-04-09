import { Component, OnInit, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';

import { UserService } from '../../users/services/user/user.service';

import { MatTableModule } from '@angular/material/table';
import { MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { PageShellComponent } from '../../../../../shared/layout/page-shell/page-shell.component';
import { DeviceApiService, DeviceDTO } from '../../../../../core/services/device-api.service';
import { BranchContextService } from '../../../../../core/services/branch-context.service';
import { MatIconModule } from '@angular/material/icon';

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
    MatIconModule
  ],
  templateUrl: './tenant-devices.component.html',
  styleUrls: ['./tenant-devices.component.scss']
})
export class TenantDevicesComponent implements OnInit {

  private api = inject(DeviceApiService);
  private branchCtx = inject(BranchContextService);
  private users = inject(UserService);

  loading = true;

  devices: DeviceDTO[] = [];
  filtered: DeviceDTO[] = [];
  paged: DeviceDTO[] = [];

  searchTerm = '';

  page = 0;
  size = 10;
  total = 0;

  userMap: Record<string, string> = {};

  displayedColumns = ['device', 'status', 'usedBy', 'lastSeen', 'actions'];

  ngOnInit() {
    this.load();
  }

  load() {

    this.loading = true;

    this.api.list(this.branchCtx.currentBranch).subscribe({
      next: data => {
        this.devices = data;
        this.resolveUsers();
        this.applySearch();
        this.loading = false;
      },
      error: () => this.loading = false
    });
  }

  resolveUsers() {

    const ids = new Set<string>();

    this.devices.forEach(d =>
      d.usedByUserIds.forEach(id => ids.add(id))
    );

    if (!ids.size) return;

    this.users.list(0, 1000).subscribe(res => {
      res.data.forEach(u => {
        if (ids.has(u.id)) {
          this.userMap[u.id] = u.username;
        }
      });
    });
  }

  getUsers(d: DeviceDTO): string {

    const names = d.usedByUserIds
      .map(id => this.userMap[id])
      .filter(Boolean);

    if (!names.length) return '—';
    if (names.length <= 2) return names.join(', ');

    return `${names[0]}, ${names[1]} +${names.length - 2}`;
  }

  applySearch() {

    const term = this.searchTerm.toLowerCase();

    this.filtered = this.devices.filter(d =>
      d.deviceName.toLowerCase().includes(term)
    );

    this.page = 0;
    this.applyPagination();
  }

  applyPagination() {

    this.total = this.filtered.length;

    const start = this.page * this.size;
    const end = start + this.size;

    this.paged = this.filtered.slice(start, end);
  }

  changePage(e: PageEvent) {
    this.page = e.pageIndex;
    this.size = e.pageSize;
    this.applyPagination();
  }

  approve(d: DeviceDTO) {
    this.api.approve(d.id).subscribe(() => this.load());
  }

  reject(d: DeviceDTO) {
    this.api.reject(d.id).subscribe(() => this.load());
  }
}