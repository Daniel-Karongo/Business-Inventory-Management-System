import { CommonModule } from '@angular/common';
import {
    Component,
    EventEmitter, inject,
    Input,
    OnInit,
    Output
} from '@angular/core';
import {
    FormArray,
    FormBuilder,
    ReactiveFormsModule,
    Validators
} from '@angular/forms';

import { MatButtonModule } from '@angular/material/button';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { ImageUploadDialogComponent } from '../../../../../../../shared/components/image-upload-dialog/image-upload-dialog.component';
import { SearchableAssignComponent } from '../../../../../../../shared/components/searchable-assign/searchable-assign.component';

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
        MatDialogModule,
        SearchableAssignComponent
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

    branchDisplay = (b: any) => b.name;
    branchId = (b: any) => b.id;

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

        const branchIds =
            (v.branchHierarchy || [])
                .map((b: any) => b.branchId);

        const primaryBranch =
            (v.branchHierarchy || [])
                .find((b: any) => b.primaryBranch);

        this.form.patchValue({
            branchIds,
            primaryBranchId: primaryBranch?.branchId ?? ''
        });

        this.syncDisabledStates();
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
        branchIds: [[] as string[]],
        primaryBranchId: ['']
    });

    ngOnInit() {

        if (!this.emails.length) this.addEmail();
        if (!this.phones.length) this.addPhone();

        this.syncDisabledStates();
    }

    get emails() {
        return this.form.get('emailAddresses') as FormArray;
    }

    get phones() {
        return this.form.get('phoneNumbers') as FormArray;
    }

    selectedBranchIds(): string[] {
        return this.form.get('branchIds')?.value ?? [];
    }

    selectedBranches(): any[] {

        const ids = this.selectedBranchIds();

        return this.branches.filter(
            b => ids.includes(b.id)
        );
    }

    private clearArray(arr: FormArray) {
        while (arr.length) {
            arr.removeAt(0);
        }
    }

    onBranchesChanged(ids: string[]) {

        this.form.get('branchIds')?.setValue(ids);

        const primary =
            this.form.get('primaryBranchId')?.value;

        if (primary && !ids.includes(primary)) {
            this.form.get('primaryBranchId')?.setValue('');
        }
    }

    private syncDisabledStates(): void {

        const selfEdit =
            this.mode === 'edit'
            && this.targetUserId === this.currentUserId;

        const editingAnotherUser =
            this.mode === 'edit'
            && !!this.targetUserId
            && !!this.currentUserId
            && this.targetUserId !== this.currentUserId;

        const roleCtrl = this.form.get('role');
        const passCtrl = this.form.get('password');
        const confirmCtrl = this.form.get('confirmPassword');

        if (selfEdit) {
            roleCtrl?.disable({ emitEvent: false });
        } else {
            roleCtrl?.enable({ emitEvent: false });
        }

        if (editingAnotherUser) {
            passCtrl?.disable({ emitEvent: false });
            confirmCtrl?.disable({ emitEvent: false });
        } else {
            passCtrl?.enable({ emitEvent: false });
            confirmCtrl?.enable({ emitEvent: false });
        }
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

        if (
            raw.branchIds?.length &&
            !raw.primaryBranchId
        ) {
            return;
        }

        this.save.emit(raw);
    }
}