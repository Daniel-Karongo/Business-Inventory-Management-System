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

}