import {
    ChangeDetectionStrategy,
    Component,
    Input
} from '@angular/core';

import { CommonModule } from '@angular/common';

@Component({
    selector:
        'app-onboarding-review-step',

    standalone: true,

    imports: [
        CommonModule
    ],

    templateUrl:
        './onboarding-review-step.component.html',

    styleUrls: [
        './onboarding-review-step.component.scss'
    ],

    changeDetection:
        ChangeDetectionStrategy.OnPush
})
export class OnboardingReviewStepComponent {

    @Input({ required: true })
    state!: any;

    get totalUnits(): number {
        return (
            this.state?.suppliers || []
        ).reduce(
            (
                sum: number,
                row: any
            ) =>
                sum +
                Number(
                    row.unitsSupplied || 0
                ),
            0
        );
    }

    get totalValue(): number {
        return (
            this.state?.suppliers || []
        ).reduce(
            (
                sum: number,
                row: any
            ) =>
                sum + (
                    Number(
                        row.unitsSupplied || 0
                    ) *
                    Number(
                        row.unitCost || 0
                    )
                ),
            0
        );
    }

    getPackagingName(
        packagingTempId: string
    ): string {

        const packaging =
            this.state?.packagings
                ?.find(
                    (p: any) =>
                        p.tempId ===
                        packagingTempId
                );

        return (
            packaging?.name ||
            'Unknown'
        );
    }

}