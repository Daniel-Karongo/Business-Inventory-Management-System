import { Component, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReactiveFormsModule, FormBuilder, Validators } from '@angular/forms';
import { Router } from '@angular/router';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import { PlatformUserApiService } from '../../services/platform-user-api.service';
import { PageShellComponent } from '../../../../../../shared/layout/page-shell/page-shell.component';
import { UserFormComponent } from '../shared/user-form/user-form.component';

@Component({
    selector: 'app-user-create',
    standalone: true,
    template:
        `
        <app-page-shell>
            <div page-content>
                <app-user-form mode="create" [loading]="loading" (save)="submit($event)" (cancel)="cancel()">
                </app-user-form>
            </div>
        </app-page-shell>
    `,
    imports: [
        CommonModule,
        ReactiveFormsModule,
        PageShellComponent,
        MatFormFieldModule,
        MatInputModule,
        MatSelectModule,
        MatButtonModule,
        MatSnackBarModule,
        UserFormComponent
    ]
})
export class UserCreateComponent {

    private fb = inject(FormBuilder);
    private api = inject(PlatformUserApiService);
    private router = inject(Router);
    private snack = inject(MatSnackBar);

    loading = false;

    form = this.fb.nonNullable.group({
        username: ['', Validators.required],
        password: ['', Validators.required],
        role: ['PLATFORM_ADMIN', Validators.required]
    });

    submit(payload: any) {
        this.loading = true;

        this.api.create(payload)
            .subscribe({
                next: () => {
                    this.snack.open(
                        'User created', 'Close',
                        { duration: 3000 }
                    );
                    this.router.navigate(['/platform/users']);
                },
                error: (err) => {
                    this.loading = false;
                    this.snack.open(
                        err?.error?.message ||
                        'Create failed',
                        'Close',
                        { duration: 6000 }
                    );

                    console.error(
                        'Platform user create error',
                        err
                    );
                }
            });
    }

    cancel() {

        this.router.navigate(
            ['/platform/users']
        );

    }

}