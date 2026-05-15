import {
    Component,
    OnInit
} from '@angular/core';
import { CommonModule } from '@angular/common';
import {
    ActivatedRoute
} from '@angular/router';
import {
    FormsModule
} from '@angular/forms';

import {
    MatIconModule
} from '@angular/material/icon';

import {
    MatButtonModule
} from '@angular/material/button';

import {
    MatProgressSpinnerModule
} from '@angular/material/progress-spinner';

import {
    MatPaginatorModule,
    PageEvent
} from '@angular/material/paginator';

import {
    MatTooltipModule
} from '@angular/material/tooltip';

import {
    WorkflowCardComponent
} from '../../../../../../shared/layout/workflow-card/workflow-card.component';

import {
    BranchAuditDTO
} from '../../models/branch.model';

import {
    BranchService
} from '../../services/branch.service';

import {
    PageWrapper
} from '../../../../../../core/models/page-wrapper.model';

@Component({
    standalone: true,
    selector: 'app-branch-audits',
    imports: [
        CommonModule,
        FormsModule,
        MatIconModule,
        MatButtonModule,
        MatPaginatorModule,
        MatProgressSpinnerModule,
        MatTooltipModule,
        WorkflowCardComponent
    ],
    templateUrl: './branch-audits.component.html',
    styleUrls: ['./branch-audits.component.scss']
})
export class BranchAuditsComponent implements OnInit {

    audits: BranchAuditDTO[] = [];

    loading = true;

    page = 0;

    size = 20;

    totalElements = 0;

    selectedAction = 'ALL';

    selectedField = 'ALL';

    readonly pageSizeOptions = [
        10,
        20,
        50,
        100
    ];

    constructor(
        private route: ActivatedRoute,
        private branchService: BranchService
    ) { }

    ngOnInit(): void {
        this.loadAudits();
    }

    loadAudits(): void {

        const branchId =
            this.route.parent?.snapshot.paramMap.get('id');

        if (!branchId) {
            this.loading = false;
            return;
        }

        this.loading = true;

        this.branchService
            .getAudits(
                branchId,
                {
                    page: this.page,
                    size: this.size
                }
            )
            .subscribe({
                next: (
                    response: PageWrapper<BranchAuditDTO>
                ) => {

                    this.audits =
                        response.content ?? [];

                    this.totalElements =
                        response.totalElements ?? 0;

                    this.loading = false;
                },
                error: () => {
                    this.loading = false;
                }
            });
    }

    onPageChange(
        event: PageEvent
    ): void {

        this.page =
            event.pageIndex;

        this.size =
            event.pageSize;

        this.loadAudits();
    }

    trackAudit(
        _: number,
        audit: BranchAuditDTO
    ): string {

        return audit.id;
    }

    /* =====================================================
       FILTERS
    ===================================================== */

    get uniqueActions(): string[] {

        return Array.from(
            new Set(
                this.audits.map(
                    a => a.action
                )
            )
        );
    }

    get uniqueFields(): string[] {

        return Array.from(
            new Set(
                this.audits
                    .map(a => a.fieldChanged)
                    .filter(
                        (
                            field
                        ): field is string => !!field
                    )
            )
        );
    }

    get filteredAudits(): BranchAuditDTO[] {

        return this.audits.filter(audit => {

            const actionMatch =
                this.selectedAction === 'ALL'
                || audit.action === this.selectedAction;

            const fieldMatch =
                this.selectedField === 'ALL'
                || audit.fieldChanged === this.selectedField;

            return actionMatch && fieldMatch;
        });
    }

    /* =====================================================
       ACTION DISPLAY
    ===================================================== */

    displayAction(
        action: string
    ): string {

        return ACTION_LABELS[action]
            ?? action
                .toLowerCase()
                .replace(/_/g, ' ')
                .replace(
                    /\b\w/g,
                    c => c.toUpperCase()
                );
    }

    getActionClass(
        action: string
    ): string {

        switch (action) {

            case 'CREATE':
                return 'create';

            case 'UPDATE':
                return 'update';

            case 'SOFT_DELETE':
            case 'HARD_DELETE':
                return 'delete';

            case 'RESTORE':
                return 'restore';

            case 'SECURITY_SETTINGS_UPDATE':
                return 'security';

            case 'ATTENDANCE_SETTINGS_UPDATE':
                return 'attendance';

            default:
                return 'default';
        }
    }

    getActionIcon(
        action: string
    ): string {

        switch (action) {

            case 'CREATE':
                return 'add_business';

            case 'UPDATE':
                return 'edit';

            case 'SOFT_DELETE':
                return 'delete';

            case 'HARD_DELETE':
                return 'delete_forever';

            case 'RESTORE':
                return 'restore';

            case 'SECURITY_SETTINGS_UPDATE':
                return 'security';

            case 'ATTENDANCE_SETTINGS_UPDATE':
                return 'schedule';

            default:
                return 'history';
        }
    }

    hasDiff(
        audit: BranchAuditDTO
    ): boolean {

        return !!(
            audit.oldValue
            || audit.newValue
        );
    }
}

const ACTION_LABELS: Record<string, string> = {

    CREATE:
        'Branch Created',

    UPDATE:
        'Branch Updated',

    SOFT_DELETE:
        'Soft Deleted',

    HARD_DELETE:
        'Permanently Deleted',

    RESTORE:
        'Restored',

    ATTENDANCE_SETTINGS_UPDATE:
        'Attendance Settings Updated',

    SECURITY_SETTINGS_UPDATE:
        'Security Settings Updated'
};