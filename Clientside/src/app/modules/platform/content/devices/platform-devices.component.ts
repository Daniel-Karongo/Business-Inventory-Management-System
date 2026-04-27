import { Component, OnInit, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { MatTableModule } from '@angular/material/table';
import { MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonToggleModule } from '@angular/material/button-toggle';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatDialog } from '@angular/material/dialog';

import { MatSnackBar } from '@angular/material/snack-bar';
import { MatFormField, MatFormFieldModule } from '@angular/material/form-field';
import { forkJoin } from 'rxjs';
import { PageShellComponent } from '../../../../shared/layout/page-shell/page-shell.component';
import { DeviceApiService, DeviceDTO } from '../../../../core/services/device-api.service';
import { RenameDeviceDialogComponent } from '../../../../shared/components/rename-device-dialog/rename-device-dialog.component';
import { DeviceAuditDialogComponent } from '../../../../shared/components/device-audit-dialog/device-audit-dialog.component';


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
        MatInputModule,
        MatFormFieldModule,
        MatIconModule,
        MatTooltipModule,
        MatButtonToggleModule,
        MatCheckboxModule
    ],

    templateUrl: './platform-devices.component.html',
    styleUrls: ['./platform-devices.component.scss']
})
export class PlatformDevicesComponent
    implements OnInit {

    private api =
        inject(DeviceApiService);

    private dialog =
        inject(MatDialog);

    private snack =
        inject(MatSnackBar);

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

    viewMode:
        'table' | 'cards' = 'table';

    selectedIds =
        new Set<string>();

    compactView = false;

    hasSelection = false;

    ngOnInit() {

        const savedView =
            localStorage.getItem(
                'platform.devices.viewMode'
            );

        const compact =
            localStorage.getItem(
                'platform.devices.compact'
            );

        if (
            savedView === 'table'
            || savedView === 'cards'
        ) {
            this.viewMode = savedView;
        }

        if (compact === 'true') {
            this.compactView = true;
            this.size = 25;
        }

        this.load();
    }

    get allVisibleSelected(): boolean {

        return this.paged.length > 0 &&
            this.paged.every(
                d => this.selectedIds.has(d.id)
            );
    }

    get partiallyVisibleSelected(): boolean {

        return this.selectedIds.size > 0
            &&
            !this.allVisibleSelected;
    }

    toggleDensity() {

        this.compactView =
            !this.compactView;

        localStorage.setItem(
            'platform.devices.compact',
            String(this.compactView)
        );

        this.size =
            this.compactView
                ? 25
                : 10;

        this.applyPagination();

    }

    setView(
        mode: 'table' | 'cards'
    ) {

        this.viewMode = mode;

        localStorage.setItem(
            'platform.devices.viewMode',
            mode
        );
    }

    load() {
        this.loading = true;
        forkJoin({
            devices: this.api.platformList(),
            stats: this.api.platformStats()
        }).subscribe({
            next: r => {
                this.devices = r.devices;
                this.stats = r.stats;
                this.applySearch();
                this.loading = false;
            },
            error: () => {
                this.loading = false;
                this.snack.open(
                    'Failed to load devices',
                    'Close',
                    { duration: 4000 }
                );
            }
        });
    }

    attempts(d: DeviceDTO) {
        this.api.platformAttempts(d.id)
            .subscribe(rows => {
                this.dialog.open(
                    DeviceAuditDialogComponent, {
                    width: 'min(900px,95vw)',
                    data: rows
                }
                );
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
                    ||
                    d.status === this.statusFilter;

                return (
                    (!term
                        || deviceMatch
                        || userMatch)
                    &&
                    statusMatch
                );

            });

        this.page = 0;

        this.applyPagination();

    }

    applyPagination() {

        this.total =
            this.filtered.length;

        const start =
            this.page * this.size;

        this.paged =
            this.filtered.slice(
                start,
                start + this.size
            );
    }

    changePage(
        e: PageEvent
    ) {

        this.page = e.pageIndex;

        this.size = e.pageSize;

        this.applyPagination();
    }

    approve(
        d: DeviceDTO
    ) {

        if (this.actionBusy) {
            return;
        }

        this.actionBusy = true;

        this.api.platformApprove(
            d.id,

            d.status === 'REJECTED'
                ?
                'Recovered rejected platform device'
                :
                'Approved by platform admin'
        )
            .subscribe({

                next: () => {

                    this.actionBusy = false;

                    this.snack.open(
                        'Device approved',
                        'Close',
                        { duration: 3000 }
                    );

                    this.load();
                },

                error: () => {

                    this.actionBusy = false;

                    this.snack.open(
                        'Approval failed',
                        'Close',
                        { duration: 3500 }
                    );
                }

            });

    }

    reject(
        d: DeviceDTO
    ) {

        if (this.actionBusy) {
            return;
        }

        this.actionBusy = true;

        this.api.platformReject(
            d.id,
            'Revoked by platform admin'
        )
            .subscribe({

                next: () => {

                    this.actionBusy = false;

                    this.snack.open(
                        'Device revoked',
                        'Close',
                        { duration: 3000 }
                    );

                    this.load();
                },

                error: () => {

                    this.actionBusy = false;

                    this.snack.open(
                        'Revoke failed',
                        'Close',
                        { duration: 3500 }
                    );
                }

            });

    }

    rename(
        d: DeviceDTO
    ) {

        const ref =
            this.dialog.open(
                RenameDeviceDialogComponent,
                {
                    width: '520px',
                    maxWidth: '95vw',
                    data: {
                        currentName: d.deviceName,
                        title: 'Rename Device',
                        subtitle: 'Update the display name used to identify this trusted device.',
                        label: 'Device Name'
                    }
                }
            );

        ref.afterClosed()
            .subscribe(name => {

                if (!name?.trim()) {
                    return;
                }

                this.api.platformRename(
                    d.id,
                    name.trim()
                ).subscribe({

                    next: () => {

                        this.snack.open(
                            'Device renamed',
                            'Close',
                            { duration: 2500 }
                        );

                        this.load();

                    },

                    error: () => {

                        this.snack.open(
                            'Rename failed',
                            'Close',
                            { duration: 3500 }
                        );

                    }

                });

            });

    }

    audit(
        d: DeviceDTO
    ) {

        this.api.platformAudit(
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

    toggleSelected(
        id: string
    ) {

        if (
            this.selectedIds.has(id)
        ) {
            this.selectedIds.delete(id);
        }
        else {
            this.selectedIds.add(id);
        }

        this.selectedIds =
            new Set(
                this.selectedIds
            );

        this.hasSelection =
            this.selectedIds.size > 0;
    }

    toggleAll() {

        const visible =
            this.paged.map(
                x => x.id
            );

        const allSelected =
            visible.every(
                id =>
                    this.selectedIds.has(id)
            );

        if (allSelected) {

            visible.forEach(
                id =>
                    this.selectedIds.delete(id)
            );

        }
        else {

            visible.forEach(
                id =>
                    this.selectedIds.add(id)
            );

        }

        this.selectedIds =
            new Set(
                this.selectedIds
            );

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
                this.api.platformApprove(
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
                this.api.platformReject(
                    id,
                    'Bulk rejected'
                ))
        ).subscribe({

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