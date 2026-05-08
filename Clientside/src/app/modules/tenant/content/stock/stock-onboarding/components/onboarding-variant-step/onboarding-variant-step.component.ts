import {
    ChangeDetectionStrategy,
    Component,
    EventEmitter,
    Output,
    inject,
    OnChanges,
    SimpleChanges,
    Input,
    OnInit
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
import { distinctUntilChanged } from 'rxjs';

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
export class OnboardingVariantStepComponent implements OnChanges {

    @Input()
    draft: any = null;

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
            .pipe(
                distinctUntilChanged(
                    (a, b) =>
                        JSON.stringify(a) ===
                        JSON.stringify(b)
                )
            )
            .subscribe(value => {

                this.valueChange.emit({
                    classification:
                        value.classification!,
                    sku: value.sku,
                    barcode: value.barcode
                });

            });

    }

    ngOnChanges(
        changes: SimpleChanges
    ) {

        if (changes['draft']) {

            const classification =
                this.draft?.classification?.trim()
                || 'STANDARD';

            this.form.patchValue({

                classification,

                sku:
                    this.draft?.sku ?? null,

                barcode:
                    this.draft?.barcode ?? null

            }, {
                emitEvent: false
            });

        }
    }
}