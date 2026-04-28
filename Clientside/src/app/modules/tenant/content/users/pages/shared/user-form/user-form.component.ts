import {
    Component, Input, Output, EventEmitter, inject, OnInit
} from '@angular/core';
import { CommonModule } from '@angular/common';
import {
    ReactiveFormsModule, FormBuilder, Validators, FormArray, FormGroup
} from '@angular/forms';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';
import { ImageUploadDialogComponent } from '../../../../../../../shared/components/image-upload-dialog/image-upload-dialog.component';

@Component({
    standalone: true,
    selector: 'app-user-form',
    imports: [
        CommonModule,
        ReactiveFormsModule,
        MatFormFieldModule,
        MatInputModule,
        MatSelectModule,
        MatButtonModule,
        MatIconModule,
        MatSlideToggleModule,
        MatDialogModule
    ],
    templateUrl: './user-form.component.html',
    styleUrls: ['./user-form.component.scss']
})
export class UserFormComponent implements OnInit {

    private fb = inject(FormBuilder);
    private dialog = inject(MatDialog);

    @Input() mode: 'create' | 'edit' = 'create';
    @Input() loading = false;

    @Input() currentUserId?: string;
    @Input() roles: string[] = [];
    @Input() branches: any[] = [];
    @Input() departments: any[] = [];

    originalRole?: string;
    targetUserId?: string;
    originalUsername?: string;

    @Input()
    set value(v: any) {

        if (!v) return;

        this.targetUserId = v.id;
        this.originalUsername = v.username;

        this.form.patchValue({
            username: v.username,
            idNumber: v.idNumber,
            role: v.role
        });

        this.originalRole = v.role;

        this.clearArray(this.emails);
        (v.emailAddresses || [])
            .forEach((x: string) =>
                this.emails.push(
                    this.fb.control(x, [Validators.email])
                ));

        this.clearArray(this.phones);
        (v.phoneNumbers || [])
            .forEach((x: string) =>
                this.phones.push(
                    this.fb.control(x)
                ));

        this.assignments.clear();

        (v.branchHierarchy || []).forEach((b: any) => {
            (b.departments || []).forEach((d: any) => {
                this.assignments.push(
                    this.newAssignment({
                        branchId: b.branchId,
                        departmentId: d.departmentId,
                        position: (d.position || 'member').toLowerCase()
                    })
                );
            });
        });
    }

    @Output() save = new EventEmitter<any>();
    @Output() cancel = new EventEmitter<void>();

    showPassword = false;
    pendingFiles: { file: File; description: string }[] = [];

    form = this.fb.group({
        username: ['', Validators.required],
        idNumber: [''],
        role: ['', Validators.required],
        password: [''],
        confirmPassword: [''],
        emailAddresses: this.fb.array([]),
        phoneNumbers: this.fb.array([]),
        departmentsAndPositions: this.fb.array([])
    });

    ngOnInit() {
        if (!this.emails.length) this.addEmail();
        if (!this.phones.length) this.addPhone();
        if (!this.assignments.length) this.addAssignment();
    }

    get emails() {
        return this.form.get('emailAddresses') as FormArray;
    }

    get phones() {
        return this.form.get('phoneNumbers') as FormArray;
    }

    get assignments() {
        return this.form.get('departmentsAndPositions') as FormArray<FormGroup>;
    }

    addEmail() {
        this.emails.push(
            this.fb.control('', [Validators.email])
        );
    }

    removeEmail(i: number) {
        this.emails.removeAt(i);
    }

    addPhone() {
        this.phones.push(
            this.fb.control('')
        );
    }

    removePhone(i: number) {
        this.phones.removeAt(i);
    }

    newAssignment(v?: any) {

        const g = this.fb.group({
            branchId: [v?.branchId || '', Validators.required],
            departmentId: [v?.departmentId || '', Validators.required],
            position: [v?.position || 'member', Validators.required]
        });

        g.get('branchId')?.valueChanges.subscribe(() => {
            g.get('departmentId')?.setValue('');
        });

        return g;
    }

    addAssignment() {
        this.assignments.push(this.newAssignment());
    }

    removeAssignment(i: number) {
        this.assignments.removeAt(i);
    }

    openFilePicker() {

        this.dialog.open(
            ImageUploadDialogComponent,
            { width: '420px' }
        )
            .afterClosed()
            .subscribe(result => {
                if (!result) return;

                this.pendingFiles.push({
                    file: result.file,
                    description: result.description
                });
            });

    }

    removePendingFile(i: number) {
        this.pendingFiles.splice(i, 1);
    }

    departmentsForBranch(branchId: string) {
        return this.departments.filter(
            (d: any) => d?.branch?.id === branchId
        );
    }

    passwordMismatch() {
        const p = this.form.get('password')?.value;
        const c = this.form.get('confirmPassword')?.value;
        return !!(p && c && p !== c);
    }

    credentialChangePending(): boolean {

        const raw = this.form.getRawValue();

        const usernameChanged =
            this.mode === 'edit' &&
            !!this.targetUserId &&
            raw.username?.trim() !== this.originalUsername;

        const passwordChanged =
            !!raw.password?.trim();

        return usernameChanged || passwordChanged;

    }

    private clearArray(arr: FormArray) {
        while (arr.length) {
            arr.removeAt(0);
        }
    }

    submit() {

        if (this.mode === 'create') {
            this.form.get('password')
                ?.addValidators(Validators.required);

            this.form.get('password')
                ?.updateValueAndValidity();
        }

        if (this.passwordMismatch()) return;

        if (this.form.invalid) {
            this.form.markAllAsTouched();
            return;
        }

        const raw: any = this.form.getRawValue();

        delete raw.confirmPassword;

        const editingAnotherUser =
            this.mode === 'edit'
            && !!this.currentUserId
            && this.targetUserId !== this.currentUserId;

        if (editingAnotherUser) {

            delete raw.password;
            delete raw.confirmPassword;

        }
        else if (
            this.mode === 'edit'
            && !raw.password?.trim()
        ) {
            delete raw.password;
        }

        raw.emailAddresses =
            (raw.emailAddresses || [])
                .filter((x: string) => x?.trim());

        raw.phoneNumbers =
            (raw.phoneNumbers || [])
                .filter((x: string) => x?.trim());

        raw.userFiles = [...this.pendingFiles];

        const selfEdit =
            this.mode === 'edit' &&
            !!this.currentUserId &&
            this.targetUserId === this.currentUserId;

        if (selfEdit) {
            delete raw.role;
        }

        this.save.emit(raw);
    }
}