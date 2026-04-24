import { Component, inject } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { MatSnackBar } from '@angular/material/snack-bar';
import { PageShellComponent } from '../../../../../../shared/layout/page-shell/page-shell.component';
import { PlatformUserApiService } from '../../services/platform-user-api.service';
import { UserFormComponent } from '../shared/user-form/user-form.component';
import { AuthService } from '../../../../../auth/services/auth.service';

@Component({
    standalone: true,
    imports: [PageShellComponent, UserFormComponent],
    template: `
        <app-page-shell>
            <div page-content>
                <app-user-form
                    mode="edit"
                    [value]="user"
                    [allowPasswordEdit]="canChangeOwnPassword"
                    [loading]="loading"
                    (save)="save($event)"
                    (cancel)="cancel()">
                </app-user-form>
            </div>
        </app-page-shell>
`
})
export class UserEditComponent {

    private api = inject(PlatformUserApiService);
    private auth = inject(AuthService);
    private route = inject(ActivatedRoute);
    private router = inject(Router);
    private snack = inject(MatSnackBar);

    id = this.route.snapshot.paramMap.get('id')!;
    user: any;
    canChangeOwnPassword = false;
    loading = false;

    constructor() {

        this.api.getById(this.id)
            .subscribe(v => {

                this.user = v;

                const me =
                    this.auth.getSnapshot();

                this.canChangeOwnPassword =
                    !!me &&
                    me.username ===
                    this.user?.username;

            });
    }

    save(payload: any) {

        this.loading = true;

        this.api.update(
            this.id,
            payload
        ).subscribe({
            next: () => {
                this.loading = false;
                this.snack.open('Platform user updated successfully', 'Close', { duration: 4000 }
                );
                this.router.navigate(['/platform/users', this.id]
                );
            },
            error: (err) => {
                this.loading = false;
                const msg = err?.error?.message || 'Update failed';
                this.snack.open(msg, 'Close', { duration: 6000 }
                );
                console.error('Platform user update error', err
                );
            }
        });

    }

    cancel() {
        this.router.navigate(
            ['/platform/users', this.id]
        );
    }
}