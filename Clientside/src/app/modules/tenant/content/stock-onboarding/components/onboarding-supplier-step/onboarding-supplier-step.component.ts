import {
    ChangeDetectionStrategy,
    Component,
    EventEmitter,
    Input,
    OnInit,
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
    MatSelectModule
} from '@angular/material/select';

import {
    SupplierService
} from '../../../suppliers/services/supplier.service';

import {
    SupplierMinimalDTO
} from '../../../suppliers/models/supplier.model';

import {
    OnboardingPackagingDraft,
    OnboardingSupplierEntry
} from '../../models/onboarding.models';

@Component({
    selector:
        'app-onboarding-supplier-step',

    standalone: true,

    imports: [
        CommonModule,
        ReactiveFormsModule,
        MatButtonModule,
        MatFormFieldModule,
        MatInputModule,
        MatSelectModule
    ],

    templateUrl:
        './onboarding-supplier-step.component.html',

    styleUrls: [
        './onboarding-supplier-step.component.scss'
    ],

    changeDetection:
        ChangeDetectionStrategy.OnPush
})
export class OnboardingSupplierStepComponent
    implements OnInit {

    private readonly fb =
        inject(FormBuilder);

    private readonly supplierService =
        inject(SupplierService);

    @Input({ required: true })
    packagings:
        OnboardingPackagingDraft[] = [];

    @Output()
    valueChange =
        new EventEmitter<
            OnboardingSupplierEntry[]
        >();

    readonly rows =
        this.fb.array<FormGroup>([]);

    suppliers:
        SupplierMinimalDTO[] = [];

    ngOnInit() {

        this.loadSuppliers();

        this.addRow();

        this.rows.valueChanges
            .subscribe(() => {

                this.emit();

            });

    }

    addRow() {

        this.rows.push(
            this.fb.group({

                supplierId: [
                    '',
                    Validators.required
                ],

                packagingTempId: [
                    '',
                    Validators.required
                ],

                quantity: [
                    1,
                    [
                        Validators.required,
                        Validators.min(1)
                    ]
                ],

                unitCost: [
                    0,
                    [
                        Validators.required,
                        Validators.min(0.01)
                    ]
                ]

            })
        );

        this.emit();

    }

    removeRow(index: number) {

        this.rows.removeAt(index);

        this.emit();

    }

    private loadSuppliers() {

        this.supplierService
            .getAll(false)
            .subscribe({
                next: suppliers => {

                    this.suppliers =
                        suppliers.map(s => ({
                            id: s.id,
                            name: s.name
                        }));

                }
            });

    }

    private emit() {

        this.valueChange.emit(
            this.rows.getRawValue() as
            OnboardingSupplierEntry[]
        );

    }

}