import {
    ChangeDetectionStrategy,
    Component,
    Input
} from '@angular/core';

import { CommonModule } from '@angular/common';

@Component({
    selector: 'app-onboarding-progress',

    standalone: true,

    imports: [
        CommonModule
    ],

    templateUrl:
        './onboarding-progress.component.html',

    styleUrls: [
        './onboarding-progress.component.scss'
    ],

    changeDetection:
        ChangeDetectionStrategy.OnPush
})
export class OnboardingProgressComponent {

    @Input({ required: true })
    currentStep!: number;

    readonly steps = [
        'Product',
        'Variant',
        'Packaging',
        'Pricing',
        'Suppliers',
        'Review'
    ];

}