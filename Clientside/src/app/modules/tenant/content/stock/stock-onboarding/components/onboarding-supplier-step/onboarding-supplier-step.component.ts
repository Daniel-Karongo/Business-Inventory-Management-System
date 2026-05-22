import {
    ChangeDetectionStrategy,
    Component,
    EventEmitter,
    Input,
    OnInit,
    Output,
    inject,
    OnChanges,
    SimpleChanges
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
} from '../../../../suppliers/services/supplier.service';

import {
    SupplierMinimalDTO
} from '../../../../suppliers/models/supplier.model';

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
export class OnboardingSupplierStepComponent implements OnInit, OnChanges {

    private readonly fb =
        inject(FormBuilder);

    private readonly supplierService =
        inject(SupplierService);

    @Input({ required: true })
    packagings:
        OnboardingPackagingDraft[] = [];

    @Input()
    value:
        OnboardingSupplierEntry[] = [];

    @Output()
    valueChange =
        new EventEmitter<
            OnboardingSupplierEntry[]
        >();

    readonly rows =
        this.fb.array<FormGroup>([]);

    suppliers:
        SupplierMinimalDTO[] = [];

    readonly NEW_SUPPLIER =
        '__NEW_SUPPLIER__';

    ngOnInit() {

        this.loadSuppliers();

        this.rows.valueChanges
            .subscribe(() => {

                this.emit();

            });

        if (!this.value.length) {

            this.addRow();

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
        row?: Partial<OnboardingSupplierEntry>
    ) {

        const group =
            this.fb.group({
                supplierId: [
                    row?.supplierId ??
                    this.NEW_SUPPLIER
                ],
                supplierName: [
                    row?.supplierName ?? ''
                ],
                packagingTempId: [
                    row?.packagingTempId ?? '',
                    Validators.required
                ],
                unitsSupplied: [
                    row?.unitsSupplied ?? 1,
                    [
                        Validators.required,
                        Validators.min(1)
                    ]
                ],
                unitCost: [
                    row?.unitCost ?? 0,
                    [
                        Validators.required,
                        Validators.min(0.01)
                    ]
                ],
                vatInclusive: [
                    row?.vatInclusive ?? true
                ],
                vatRate: [
                    row?.vatRate ?? 16,
                    [
                        Validators.required,
                        Validators.min(0),
                        Validators.max(100)
                    ]
                ]
            });

        group.valueChanges
            .subscribe(value => {

                const supplierId =
                    value.supplierId;

                const supplierName =
                    value.supplierName;

                if (
                    supplierId ===
                    this.NEW_SUPPLIER &&
                    !supplierName?.trim()
                ) {

                    group
                        .get('supplierId')
                        ?.setErrors({
                            required: true
                        });

                }

            });

        this.rows.push(group);

    }

    removeRow(index: number) {

        this.rows.removeAt(index);

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

    isCreatingSupplier(index: number): boolean {

        return (
            this.rows.at(index)
                ?.get('supplierId')
                ?.value === this.NEW_SUPPLIER
        );

    }

    private rebuild() {

        this.rows.clear({
            emitEvent: false
        });

        for (const row of this.value) {

            this.addRow(row);

        }

    }

    private emit() {
        const value =
            this.rows.getRawValue() as any[];

        this.valueChange.emit(
            value.map(row => ({
                ...row,
                supplierId:
                    row.supplierId ===
                        this.NEW_SUPPLIER
                        ? null
                        : row.supplierId,
                unitsSupplied:
                    Number(row.unitsSupplied),
                unitCost:
                    Number(row.unitCost),
                vatRate:
                    Number(row.vatRate),
                vatInclusive:
                    !!row.vatInclusive
            }))
        );
    }
}