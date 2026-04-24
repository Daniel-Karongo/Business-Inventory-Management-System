import {
    Component,
    Input,
    Output,
    EventEmitter,
    inject
} from '@angular/core';

import { CommonModule } from '@angular/common';

import {
    ReactiveFormsModule,
    FormBuilder,
    Validators,
    FormArray
} from '@angular/forms';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';

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
        MatSlideToggleModule
    ],
    templateUrl: './user-form.component.html',
    styleUrls: ['./user-form.component.scss']
})
export class UserFormComponent {

    private fb = inject(FormBuilder);

    @Input()
    mode: 'create' | 'edit' = 'create';

    @Input()
    loading = false;

    @Input()
    allowPasswordEdit = false;

    @Input()
    set value(v: any) {

        if (!v) return;

        this.form.patchValue({
            username: v.username, role: v.role, active: v.active, locked: v.locked, mustChangePassword: v.mustChangePassword, idNumber: v.idNumber
        });

        this.clearArray(this.emails
        );

        (v.emailAddresses || []).forEach((x: string) => this.emails.push(this.fb.control(x, [Validators.email])));

        this.clearArray(this.phones
        );

        (v.phoneNumbers || []).forEach((x: string) => this.phones.push(this.fb.control(x, [])));

    }

    @Output()
    save =
        new EventEmitter<any>();

    @Output()
    cancel =
        new EventEmitter<void>();

    showPassword = false;

    showResetPassword = false;

    form = this.fb.nonNullable.group({
        username: ['', [Validators.required, Validators.minLength(3)]
        ],
        password: [''],
        confirmPassword: [''],
        role: ['PLATFORM_ADMIN', Validators.required
        ],
        active: [true],
        locked: [false],
        mustChangePassword: [true],
        idNumber: [''],
        emailAddresses: this.fb.array([]),
        phoneNumbers: this.fb.array([])
    });

    get emails() {
        return this.form.get('emailAddresses'
        ) as FormArray;
    }

    get phones() {
        return this.form.get('phoneNumbers'
        ) as FormArray;
    }

    addEmail() {

        this.emails.push(this.fb.control('', [Validators.email])
        );

    }

    removeEmail(i: number) {
        this.emails.removeAt(i);
    }

    addPhone() {
        this.phones.push(this.fb.control('')
        );
    }

    removePhone(i: number) {
        this.phones.removeAt(i);
    }

    private clearArray(
        arr: FormArray
    ) {
        while (arr.length) {
            arr.removeAt(0);
        }
    }

    passwordMismatch(): boolean {

        const p =
            this.form.controls.password.value;

        const c =
            this.form.controls.confirmPassword.value;

        return !!(
            p &&
            c &&
            p !== c
        );
    }

    submit() {

        if (this.mode === 'create'
        ) {
            this.form.controls.password.addValidators(Validators.required);
            this.form.controls.password.updateValueAndValidity();

        }

        if (this.passwordMismatch()) {
            this.form.controls.confirmPassword
                .markAsTouched();
            return;
        }

        if (this.form.invalid
        ) {
            this.form.markAllAsTouched(); return;
        }

        const raw: any = this.form.getRawValue();

        delete raw.confirmPassword;

        if (this.mode === 'edit') {
            if (!raw.password?.trim()) {
                delete raw.password;
            }
        }

        raw.emailAddresses = (raw.emailAddresses || []).filter((x: string) => x?.trim());

        raw.phoneNumbers = (raw.phoneNumbers || []).filter((x: string) => x?.trim());
        this.save.emit(raw);

    }
}