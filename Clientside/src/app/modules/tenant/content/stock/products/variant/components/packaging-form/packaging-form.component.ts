import { CommonModule } from '@angular/common';
import {
    Component,
    EventEmitter,
    Inject,
    Output
} from '@angular/core';

import {
    FormBuilder,
    ReactiveFormsModule,
    Validators
} from '@angular/forms';

import { MAT_DIALOG_DATA, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatCheckboxModule } from '@angular/material/checkbox';

import { PackagingDTO } from '../../../../models/packaging.model';

@Component({
    selector: 'app-packaging-form',
    standalone: true,
    imports: [
        CommonModule,
        ReactiveFormsModule,
        MatDialogModule,
        MatButtonModule,
        MatInputModule,
        MatCheckboxModule
    ],
    templateUrl: './packaging-form.component.html',
    styleUrls: ['./packaging-form.component.scss']
})
export class PackagingFormComponent {

    @Output() saved = new EventEmitter<any>();

    saving = false;

    form!: ReturnType<FormBuilder['group']>;

    constructor(
        private fb: FormBuilder,
        private ref: MatDialogRef<PackagingFormComponent>,
        @Inject(MAT_DIALOG_DATA)
        public data: {
            packaging?: PackagingDTO;
            editMode?: boolean;
        }
    ) {
        this.form = this.fb.group({
            name: ['', [Validators.required, Validators.maxLength(100)]],
            unitsPerPackaging: [
                1,
                [Validators.required, Validators.min(1)]
            ]
        });
        
        if (data?.packaging) {

            this.form.patchValue({
                name: data.packaging.name,
                unitsPerPackaging: data.packaging.unitsPerPackaging
            });

            if (data.packaging.isBaseUnit) {
                this.form.get('unitsPerPackaging')?.disable();
            }
        }
    }

    submit() {

        if (this.form.invalid || this.saving) {
            this.form.markAllAsTouched();
            return;
        }

        this.saving = true;

        this.ref.close(this.form.getRawValue());
    }

    close() {
        this.ref.close();
    }
}