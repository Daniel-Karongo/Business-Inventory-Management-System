import {
    ChangeDetectionStrategy,
    Component,
    EventEmitter,
    Input,
    OnChanges,
    OnInit,
    Output,
    SimpleChanges,
    inject
} from '@angular/core';

import { CommonModule } from '@angular/common';

import {
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
    implements OnInit, OnChanges {

    private readonly fb =
        inject(FormBuilder);

    @Input({ required: true })
    packagings:
        OnboardingPackagingDraft[] = [];

    @Input()
    value:
        OnboardingPricingDraft[] = [];

    @Output()
    valueChange =
        new EventEmitter<
            OnboardingPricingDraft[]
        >();

    readonly rows =
        this.fb.array<FormGroup>([]);

    ngOnInit() {

        this.rows.valueChanges
            .subscribe(() => {

                this.emit();

            });

    }

    ngOnChanges(
        changes: SimpleChanges
    ) {

        if (
            changes['packagings'] &&
            this.rows.length === 0
        ) {

            this.rebuild();

        }

    }

    private rebuild() {

        this.rows.clear({
            emitEvent: false
        });

        for (const packaging of this.packagings) {

            const existing =
                this.value.find(
                    x =>
                        x.packagingTempId ===
                        packaging.tempId
                );

            this.rows.push(

                this.fb.group({

                    packagingTempId: [
                        packaging.tempId
                    ],

                    packagingName: [
                        packaging.name
                    ],

                    sellingPrice: [
                        existing?.sellingPrice ?? null,
                        [
                            Validators.required,
                            Validators.min(0.01)
                        ]
                    ]

                })

            );

        }

    }

    private emit() {

        this.valueChange.emit(

            this.rows.getRawValue() as
            OnboardingPricingDraft[]

        );

    }

}