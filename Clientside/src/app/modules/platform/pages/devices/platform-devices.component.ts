import { Component, OnInit, inject, ViewChild } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';

import { DomainContextService } from '../../../../core/services/domain-context.service';
import { BranchContextService } from '../../../../core/services/branch-context.service';

import { PageShellComponent } from '../../../../shared/layout/page-shell/page-shell.component';

import { MatTableModule } from '@angular/material/table';
import { MatPaginator, MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { DeviceApiService, DeviceDTO } from '../../../../core/services/device-api.service';
import { UserService } from '../../../tenant/content/users/services/user/user.service';
import { PlatformUserApiService } from '../../users/services/platform-user-api.service';

@Component({
    standalone: true,
    selector: 'app-platform-devices',
    imports: [
        CommonModule,
        FormsModule,
        PageShellComponent,
        MatTableModule,
        MatPaginatorModule,
        MatButtonModule,
        MatInputModule
    ],
    templateUrl: './platform-devices.component.html',
    styleUrls: ['./platform-devices.component.scss']
})
export class PlatformDevicesComponent implements OnInit {

    private api = inject(DeviceApiService);
    private domain = inject(DomainContextService);
    private platformUsers = inject(PlatformUserApiService);

    @ViewChild(MatPaginator) paginator?: MatPaginator;

    loading = true;

    devices: DeviceDTO[] = [];
    filtered: DeviceDTO[] = [];
    paged: DeviceDTO[] = [];

    searchTerm = '';

    page = 0;
    size = 10;
    total = 0;

    displayedColumns = [
        'device',
        'status',
        'usedBy',
        'lastSeen',
        'actions'
    ];

    userMap: Record<string, string> = {};

    ngOnInit() {
        this.load();
    }

    load() {

        this.loading = true;

        this.api.list(null).subscribe({
            next: data => {
                this.devices = data;
                this.resolveUsers();
                this.applySearch();
                this.loading = false;
            },
            error: () => this.loading = false
        });
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

    resolveUsers() {

        const ids = new Set<string>();

        this.devices.forEach(d =>
            d.usedByUserIds.forEach(id => ids.add(id))
        );

        if (ids.size === 0) return;

        this.platformUsers.listAll().subscribe(users => {
            users.forEach((u: any) => {
                if (ids.has(u.id)) {
                    this.userMap[u.id] = u.username;
                }
            });
        });
    }
}