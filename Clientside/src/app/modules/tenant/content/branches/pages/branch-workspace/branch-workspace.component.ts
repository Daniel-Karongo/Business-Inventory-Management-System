import {
    Component,
    OnInit
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
export class BranchWorkspaceComponent implements OnInit {

    branch?: BranchDetailsDTO;

    loading = true;

    isMobile = false;

    readonly tabs = [
        {
            label: 'Overview',
            route: 'overview',
            icon: 'visibility'
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
        private breakpointObserver: BreakpointObserver
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

        this.branchService
            .getById(id)
            .subscribe({
                next: branch => {

                    this.branch = branch;

                    this.loading = false;
                },
                error: () => {

                    this.loading = false;
                }
            });
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
}