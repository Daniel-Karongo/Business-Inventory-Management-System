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
export class OnboardingPackagingStepComponent
    implements OnInit, OnChanges {

    private readonly fb =
        inject(FormBuilder);

    @Input()
    value:
        OnboardingPackagingDraft[] = [];

    @Output()
    valueChange =
        new EventEmitter<
            OnboardingPackagingDraft[]
        >();

    readonly rows =
        this.fb.array<FormGroup>([]);

    ngOnInit() {

        this.rows.valueChanges
            .subscribe(() => {

                this.emit();

            });

        /*
          INITIAL EMPTY STATE
        */

        if (!this.value.length) {

            this.addRow(
                'Piece',
                1,
                true
            );

        }

    }

    ngOnChanges(
        changes: SimpleChanges
    ) {

        if (
            changes['value'] &&
            this.rows.length === 0 &&
            this.value?.length
        ) {

            this.rebuild();

        }

    }

    addRow(
        name = '',
        units = 1,
        base = false,
        barcode = '',
        tempId = crypto.randomUUID()
    ) {

        this.rows.push(

            this.fb.group({

                tempId: [tempId],

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

                barcode: [barcode],

                isBaseUnit: [base]

            })

        );
    }

    removeRow(index: number) {

        if (index === 0) {
            return;
        }

        this.rows.removeAt(index);
    }

    private rebuild() {

        this.rows.clear({
            emitEvent: false
        });

        for (const row of this.value) {

            this.rows.push(

                this.fb.group({

                    tempId: [row.tempId],

                    name: [
                        row.name,
                        Validators.required
                    ],

                    unitQuantity: [
                        row.unitQuantity,
                        [
                            Validators.required,
                            Validators.min(1)
                        ]
                    ],

                    barcode: [
                        row.barcode ?? ''
                    ],

                    isBaseUnit: [
                        row.isBaseUnit
                    ]

                })

            );

        }

    }

    private emit() {

        this.valueChange.emit(

            this.rows.getRawValue() as
            OnboardingPackagingDraft[]

        );

    }

}