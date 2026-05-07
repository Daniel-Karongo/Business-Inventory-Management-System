import {
    ChangeDetectionStrategy,
    Component,
    EventEmitter,
    Input,
    OnChanges,
    Output,
    SimpleChanges,
    inject
} from '@angular/core';

import { CommonModule } from '@angular/common';

import {
    FormArray,
    FormBuilder,
    FormGroup,
    ReactiveFormsModule,
    Validators
} from '@angular/forms';

import {
    MatFormFieldModule
} from '@angular/material/form-field';

import {
    MatInputModule
} from '@angular/material/input';

import {
    OnboardingPackagingDraft,
    OnboardingPricingDraft
} from '../../models/onboarding.models';

@Component({
    selector:
        'app-onboarding-pricing-step',

    standalone: true,

    imports: [
        CommonModule,
        ReactiveFormsModule,
        MatFormFieldModule,
        MatInputModule
    ],

    templateUrl:
        './onboarding-pricing-step.component.html',

    styleUrls: [
        './onboarding-pricing-step.component.scss'
    ],

    changeDetection:
        ChangeDetectionStrategy.OnPush
})
export class OnboardingPricingStepComponent
    implements OnChanges {

    private readonly fb =
        inject(FormBuilder);

    @Input({ required: true })
    packagings:
        OnboardingPackagingDraft[] = [];

    @Output()
    valueChange =
        new EventEmitter<
            OnboardingPricingDraft[]
        >();

    readonly rows =
        this.fb.array<FormGroup>([]);

    ngOnChanges(
        changes: SimpleChanges
    ) {

        if (
            changes['packagings']
        ) {

            this.rebuild();

        }

    }

    private rebuild() {

        this.rows.clear();

        for (
            const packaging
            of this.packagings
        ) {

            this.rows.push(
                this.fb.group({

                    packagingTempId: [
                        packaging.tempId
                    ],

                    packagingName: [
                        packaging.name
                    ],

                    sellingPrice: [
                        null,
                        [
                            Validators.required,
                            Validators.min(0.01)
                        ]
                    ]

                })
            );

        }

        this.rows.valueChanges
            .subscribe(() => {

                this.emit();

            });

        this.emit();

    }

    private emit() {

        this.valueChange.emit(
            this.rows.getRawValue() as
            OnboardingPricingDraft[]
        );

    }

}