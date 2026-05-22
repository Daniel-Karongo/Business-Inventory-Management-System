import {
    ChangeDetectionStrategy,
    Component,
    Input
} from '@angular/core';

import { CommonModule }
    from '@angular/common';

import { MatProgressSpinnerModule }
    from '@angular/material/progress-spinner';

@Component({
    selector: 'app-enterprise-table',
    standalone: true,
    imports: [
        CommonModule,
        MatProgressSpinnerModule
    ],
    templateUrl:
        './enterprise-table.component.html',
    styleUrls: [
        './enterprise-table.component.scss'
    ],
    changeDetection:
        ChangeDetectionStrategy.OnPush
})
export class EnterpriseTableComponent {

    @Input()
    loading = false;

    @Input()
    hasData = false;

    @Input()
    emptyMessage =
        'No records found';

    @Input()
    error: string | null = null;

}