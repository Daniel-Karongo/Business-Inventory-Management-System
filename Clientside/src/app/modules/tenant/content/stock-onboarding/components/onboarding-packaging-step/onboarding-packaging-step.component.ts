import {
    ChangeDetectionStrategy,
    Component,
    EventEmitter,
    Output,
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
    MatButtonModule
} from '@angular/material/button';

import {
    MatFormFieldModule
} from '@angular/material/form-field';

import {
    MatInputModule
} from '@angular/material/input';

import {
    OnboardingPackagingDraft
} from '../../models/onboarding.models';

@Component({
    selector:
        'app-onboarding-packaging-step',

    standalone: true,

    imports: [
        CommonModule,
        ReactiveFormsModule,
        MatButtonModule,
        MatFormFieldModule,
        MatInputModule
    ],

    templateUrl:
        './onboarding-packaging-step.component.html',

    styleUrls: [
        './onboarding-packaging-step.component.scss'
    ],

    changeDetection:
        ChangeDetectionStrategy.OnPush
})
export class OnboardingPackagingStepComponent {

    private readonly fb =
        inject(FormBuilder);

    @Output()
    valueChange =
        new EventEmitter<
            OnboardingPackagingDraft[]
        >();

    readonly rows =
        this.fb.array<FormGroup>([]);

    constructor() {

        this.addRow(
            'Piece',
            1,
            true
        );

        this.rows.valueChanges
            .subscribe(() => {

                this.emit();

            });

    }

    addRow(
        name = '',
        units = 1,
        base = false
    ) {

        const row =
            this.fb.group({

                tempId: [
                    crypto.randomUUID()
                ],

                name: [
                    name,
                    Validators.required
                ],

                unitQuantity: [
                    units,
                    [
                        Validators.required,
                        Validators.min(1)
                    ]
                ],

                barcode: [''],

                isBaseUnit: [base]

            });

        this.rows.push(row);

        this.emit();

    }

    removeRow(index: number) {

        if (index === 0) {
            return;
        }

        this.rows.removeAt(index);

        this.emit();

    }

    private emit() {

        this.valueChange.emit(
            this.rows.getRawValue() as
            OnboardingPackagingDraft[]
        );

    }

}