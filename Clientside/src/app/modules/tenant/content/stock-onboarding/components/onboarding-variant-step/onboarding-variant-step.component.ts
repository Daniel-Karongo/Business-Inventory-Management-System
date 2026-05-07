import {
    ChangeDetectionStrategy,
    Component,
    EventEmitter,
    Output,
    inject
} from '@angular/core';

import { CommonModule } from '@angular/common';

import {
    FormBuilder,
    ReactiveFormsModule,
    Validators
} from '@angular/forms';

import {
    MatFormFieldModule
} from '@angular/material/form-field';

import {
    MatInputModule
} from '@angular/material/input';

export interface VariantStepValue {
    classification: string;
    sku?: string | null;
    barcode?: string | null;
}

@Component({
    selector:
        'app-onboarding-variant-step',

    standalone: true,

    imports: [
        CommonModule,
        ReactiveFormsModule,
        MatFormFieldModule,
        MatInputModule
    ],

    templateUrl:
        './onboarding-variant-step.component.html',

    styleUrls: [
        './onboarding-variant-step.component.scss'
    ],

    changeDetection:
        ChangeDetectionStrategy.OnPush
})
export class OnboardingVariantStepComponent {

    private readonly fb =
        inject(FormBuilder);

    @Output()
    valueChange =
        new EventEmitter<
            VariantStepValue
        >();

    readonly form =
        this.fb.group({

            classification: [
                'STANDARD',
                Validators.required
            ],

            sku: [''],

            barcode: ['']

        });

    constructor() {

        this.form.valueChanges
            .subscribe(value => {

                this.valueChange.emit({
                    classification:
                        value.classification!,
                    sku: value.sku,
                    barcode: value.barcode
                });

            });

    }

}