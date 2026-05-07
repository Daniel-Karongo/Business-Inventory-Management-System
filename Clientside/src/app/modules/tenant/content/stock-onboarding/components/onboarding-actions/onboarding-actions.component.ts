import {
    Component,
    EventEmitter,
    Input,
    Output
} from '@angular/core';

import { CommonModule } from '@angular/common';

import {
    MatButtonModule
} from '@angular/material/button';

@Component({
    selector: 'app-onboarding-actions',

    standalone: true,

    imports: [
        CommonModule,
        MatButtonModule
    ],

    templateUrl:
        './onboarding-actions.component.html',

    styleUrls: [
        './onboarding-actions.component.scss'
    ]
})
export class OnboardingActionsComponent {

    @Input()
    disableNext = false;

    @Input()
    disablePrevious = false;

    @Input()
    isLastStep = false;

    @Output()
    previous =
        new EventEmitter<void>();

    @Output()
    next =
        new EventEmitter<void>();

    @Output()
    submit =
        new EventEmitter<void>();

}