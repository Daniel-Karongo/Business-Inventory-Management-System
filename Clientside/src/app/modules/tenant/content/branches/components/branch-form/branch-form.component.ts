import {
    Component,
    EventEmitter,
    Input,
    Output,
    OnInit
} from '@angular/core';

import { CommonModule } from '@angular/common';

import {
    FormBuilder,
    ReactiveFormsModule,
    Validators
} from '@angular/forms';

import { MatButtonModule } from '@angular/material/button';

import { MatInputModule } from '@angular/material/input';

import { MatFormFieldModule } from '@angular/material/form-field';

import { MatExpansionModule } from '@angular/material/expansion';

import {
    SearchableAssignComponent
} from '../../../../../../shared/components/searchable-assign/searchable-assign.component';

import {
    MinimalUserDTO
} from '../../../users/models/user.model';

import {
    BranchFormDTO,
    BranchDetailsDTO
} from '../../models/branch.model';

@Component({
    standalone: true,
    selector: 'app-branch-form',
    imports: [
        CommonModule,
        ReactiveFormsModule,
        MatButtonModule,
        MatInputModule,
        MatFormFieldModule,
        MatExpansionModule,
        SearchableAssignComponent
    ],
    templateUrl: './branch-form.component.html',
    styleUrls: ['./branch-form.component.scss']
})
export class BranchFormComponent implements OnInit {

    @Input()
    initial?: Partial<BranchDetailsDTO>;

    @Input()
    loading = false;

    @Input()
    submitLabel = 'Save';

    @Input()
    users: MinimalUserDTO[] = [];

    @Input()
    userIds: string[] = [];

    @Output()
    userIdsChange =
        new EventEmitter<string[]>();

    readonly userDisplay =
        (u: MinimalUserDTO) => u.username;

    readonly userId =
        (u: MinimalUserDTO) => u.id;

    @Output()
    submitted =
        new EventEmitter<BranchFormDTO>();

    form!: ReturnType<
        BranchFormComponent['createForm']
    >;

    constructor(
        private fb: FormBuilder
    ) {

        this.form =
            this.createForm();
    }

    ngOnInit() {

        if (!this.initial) {
            return;
        }

        this.form.patchValue({
            branchCode:
                this.initial.branchCode ?? '',

            name:
                this.initial.name ?? '',

            location:
                this.initial.location ?? '',

            phone:
                this.initial.phone ?? '',

            email:
                this.initial.email ?? '',

            maxActiveSessionsPerUser:
                this.initial.maxActiveSessionsPerUser ?? 4
        });
    }

    private createForm() {

        return this.fb.nonNullable.group({

            branchCode: [
                '',
                [
                    Validators.required,
                    Validators.maxLength(30)
                ]
            ],

            name: [
                '',
                [
                    Validators.required,
                    Validators.maxLength(120)
                ]
            ],

            location: [
                '',
                [
                    Validators.maxLength(255)
                ]
            ],

            phone: [
                '',
                [
                    Validators.maxLength(30)
                ]
            ],

            email: [
                '',
                [
                    Validators.email,
                    Validators.maxLength(120)
                ]
            ],

            maxActiveSessionsPerUser: [
                4,
                [
                    Validators.min(1)
                ]
            ]
        });
    }

    submit() {

        if (this.form.invalid) {

            this.form.markAllAsTouched();

            return;
        }

        this.submitted.emit({
            ...this.form.getRawValue()
        });
    }
}