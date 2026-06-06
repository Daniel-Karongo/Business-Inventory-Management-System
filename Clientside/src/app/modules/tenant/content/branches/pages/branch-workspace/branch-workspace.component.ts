import {
    Component,
    OnDestroy,
    OnInit,
    ViewChild
} from '@angular/core';

import {
    CommonModule
} from '@angular/common';

import {
    ActivatedRoute,
    Router,
    RouterModule
} from '@angular/router';

import {
    BreakpointObserver
} from '@angular/cdk/layout';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatIconModule
} from '@angular/material/icon';

import {
    MatMenuModule
} from '@angular/material/menu';

import {
    WorkflowShellComponent
} from '../../../../../../shared/layout/workflow-shell/workflow-shell.component';

import {
    BranchService
} from '../../services/branch.service';

import {
    BranchDetailsDTO
} from '../../models/branch.model';
import { Subject, takeUntil } from 'rxjs';
import { BranchWorkspaceStateService } from '../../services/branch-workspace-state.service';
import { environment } from '../../../../../../../environments/environment';
import { EntityImageManagerComponent } from '../../../../../../shared/components/entity-image-manager/entity-image-manager.component';
import { FileViewerDialog } from '../../../../../../shared/components/file-viewer/file-viewer.component';
import { MatDialog } from '@angular/material/dialog';

@Component({
    standalone: true,
    selector: 'app-branch-workspace',
    imports: [
        CommonModule,
        RouterModule,
        MatButtonModule,
        MatIconModule,
        MatMenuModule,
        WorkflowShellComponent
    ],
    templateUrl: './branch-workspace.component.html',
    styleUrls: ['./branch-workspace.component.scss']
})
export class BranchWorkspaceComponent implements OnInit, OnDestroy {

    @ViewChild(EntityImageManagerComponent)
    private imageManager?: EntityImageManagerComponent;

    branch?: BranchDetailsDTO;

    loading = true;

    isMobile = false;

    private readonly destroy$ =
        new Subject<void>();

    logoObjectUrl?: string;

    readonly tabs = [
        {
            label: 'Overview',
            route: 'overview',
            icon: 'visibility'
        },

        {
            label: 'Documents',
            route: 'images',
            icon: 'folder'
        },

        {
            label: 'Security',
            route: 'security',
            icon: 'shield'
        },
        {
            label: 'Attendance',
            route: 'attendance',
            icon: 'schedule'
        },
        {
            label: 'Notifications',
            route: 'notifications',
            icon: 'notifications'
        },
        {
            label: 'Email',
            route: 'email',
            icon: 'mail'
        },
        {
            label: 'SMS',
            route: 'sms',
            icon: 'sms'
        },
        {
            label: 'M-Pesa',
            route: 'mpesa',
            icon: 'payments'
        },
        {
            label: 'Audits',
            route: 'audits',
            icon: 'history'
        }
    ];

    constructor(
        private route: ActivatedRoute,
        private router: Router,
        private branchService: BranchService,
        private breakpointObserver: BreakpointObserver,
        private workspaceState: BranchWorkspaceStateService,
        private dialog: MatDialog
    ) { }

    ngOnInit(): void {

        this.breakpointObserver
            .observe('(max-width: 768px)')
            .subscribe(result => {

                this.isMobile =
                    result.matches;
            });

        const id =
            this.route.snapshot.paramMap.get('id');

        if (!id) {

            this.loading = false;

            return;
        }

        this.loadBranch(id);

        this.workspaceState.branch$
            .pipe(
                takeUntil(this.destroy$)
            )
            .subscribe(branch => {
                if (branch) {
                    this.branch = branch;
                    this.loadLogo(branch);
                }
            });
    }

    ngOnDestroy(): void {
        if (this.logoObjectUrl) {
            URL.revokeObjectURL(this.logoObjectUrl);
        }

        this.destroy$.next();
        this.destroy$.complete();
    }

    private loadBranch(
        id: string
    ): void {
        this.branchService
            .getById(id)
            .subscribe({
                next: branch => {
                    this.branch = branch;

                    this.loadLogo(branch);

                    this.loading = false;

                    this.workspaceState.setBranch(branch);
                },
                error: () => {
                    this.loading = false;
                }
            });
    }

    private loadLogo(
        branch: BranchDetailsDTO
    ): void {
        if (!branch.logoUrl) {
            this.logoObjectUrl = undefined;
            return;
        }

        const fileName =
            decodeURIComponent(
                branch.logoUrl.substring(
                    branch.logoUrl.lastIndexOf('/') + 1
                )
            );

        this.branchService
            .downloadDocument(
                branch.id,
                fileName
            )
            .subscribe({
                next: blob => {

                    if (this.logoObjectUrl) {
                        URL.revokeObjectURL(
                            this.logoObjectUrl
                        );
                    }

                    this.logoObjectUrl =
                        URL.createObjectURL(blob);
                },
                error: () => {
                    this.logoObjectUrl =
                        undefined;
                }
            });
    }

    openLogo(): void {
        if (!this.logoObjectUrl || !this.branch) {
            return;
        }

        this.dialog.open(
            FileViewerDialog,
            {
                panelClass: 'enterprise-dialog',
                width: '90vw',
                maxWidth: '90vw',
                height: '90vh',
                maxHeight: '90vh',
                data: {
                    preview: {
                        src: this.logoObjectUrl,
                        name: 'Branch Logo',
                        type: 'image'
                    }
                }
            }
        );
    }

    openLogoOrUpload(): void {

        if (this.logoObjectUrl) {
            this.openLogo();
            return;
        }

        const currentUrl =
            this.router.url;

        if (!currentUrl.endsWith('/images')) {

            this.router.navigate(
                ['images'],
                {
                    relativeTo: this.route
                }
            );
        }

        setTimeout(() => {
            this.workspaceState
                .openUploadDialog();
        }, 250);
    }

    isActive(
        route: string
    ): boolean {

        return this.router.url.includes(
            `/${route}`
        );
    }

    trackTab(
        _: number,
        tab: {
            route: string;
        }
    ): string {

        return tab.route;
    }

    get statusClass(): string {

        if (!this.branch) {
            return 'loading';
        }

        return this.branch.deleted
            ? 'deleted'
            : 'active';
    }

    get statusLabel(): string {

        if (!this.branch) {
            return 'Loading';
        }

        return this.branch.deleted
            ? 'Deleted'
            : 'Active';
    }

    get logoUrl(): string | null {
        if (!this.branch?.logoUrl) {
            return null;
        }

        return `${environment.apiUrl}${this.branch.logoUrl}`;
    }
}