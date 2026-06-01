import {
    Component,
    Input,
    OnInit
} from '@angular/core';

import {
    CommonModule
} from '@angular/common';

import {
    FormsModule
} from '@angular/forms';

import {
    MatIconModule
} from '@angular/material/icon';

import {
    MatProgressSpinnerModule
} from '@angular/material/progress-spinner';

import {
    VariantAudit
} from '../../../../models/product-variant.model';

import {
    ProductVariantService
} from '../../services/product-variant.service';

@Component({
    selector: 'app-variant-audits',

    standalone: true,

    imports: [
        CommonModule,
        FormsModule,
        MatIconModule,
        MatProgressSpinnerModule
    ],

    templateUrl:
        './variant-audits.component.html',

    styleUrls: [
        './variant-audits.component.scss'
    ]
})
export class VariantAuditsComponent
    implements OnInit {

    @Input({
        required: true
    })
    variantId!: string;

    @Input()
    branchId?: string;

    audits: VariantAudit[] = [];

    loading = true;

    selectedAction = 'ALL';

    selectedField = 'ALL';

    constructor(
        private variantService:
            ProductVariantService
    ) { }

    ngOnInit(): void {
        this.loadAudits();
    }

    loadAudits(): void {

        this.loading = true;

        this.variantService
            .getAuditHistory(
                this.variantId,
                this.branchId
            )
            .subscribe({
                next: audits => {

                    this.audits =
                        audits ?? [];

                    this.loading =
                        false;
                },

                error: () => {

                    this.loading =
                        false;
                }
            });
    }

    trackAudit(
        _: number,
        audit: VariantAudit
    ): string {

        return (
            audit.timestamp +
            audit.action +
            (
                audit.fieldChanged ??
                ''
            )
        );
    }

    get uniqueActions():
        string[] {

        return Array.from(
            new Set(
                this.audits.map(
                    a => a.action
                )
            )
        );
    }

    get uniqueFields():
        string[] {

        return Array.from(
            new Set(
                this.audits
                    .map(
                        a => a.fieldChanged
                    )
                    .filter(
                        (
                            field
                        ): field is string =>
                            !!field
                    )
            )
        );
    }

    get filteredAudits():
        VariantAudit[] {

        return this.audits.filter(
            audit => {

                const actionMatch =
                    this.selectedAction ===
                    'ALL'
                    ||
                    audit.action ===
                    this.selectedAction;

                const fieldMatch =
                    this.selectedField ===
                    'ALL'
                    ||
                    audit.fieldChanged ===
                    this.selectedField;

                return (
                    actionMatch
                    &&
                    fieldMatch
                );
            }
        );
    }

    displayAction(
        action: string
    ): string {

        return ACTION_LABELS[
            action
        ]
            ??
            action
                .toLowerCase()
                .replace(
                    /_/g,
                    ' '
                )
                .replace(
                    /\b\w/g,
                    c =>
                        c.toUpperCase()
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

            default:
                return 'default';
        }
    }

    getActionIcon(
        action: string
    ): string {

        switch (action) {

            case 'CREATE':
                return 'add_box';

            case 'UPDATE':
                return 'edit';

            case 'SOFT_DELETE':
                return 'delete';

            case 'HARD_DELETE':
                return 'delete_forever';

            case 'RESTORE':
                return 'restore';

            default:
                return 'history';
        }
    }

    hasDiff(
        audit: VariantAudit
    ): boolean {

        return !!(
            audit.oldValue
            ||
            audit.newValue
        );
    }
}

const ACTION_LABELS:
    Record<string, string> = {

    CREATE:
        'Variant Created',

    UPDATE:
        'Variant Updated',

    SOFT_DELETE:
        'Soft Deleted',

    HARD_DELETE:
        'Permanently Deleted',

    RESTORE:
        'Restored'
};