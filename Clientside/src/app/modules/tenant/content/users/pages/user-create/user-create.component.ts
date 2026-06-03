import { Component, inject } from '@angular/core';
import { Router } from '@angular/router';
import { MatSnackBar } from '@angular/material/snack-bar';

import { PageShellComponent } from '../../../../../../shared/layout/page-shell/page-shell.component';
import { UserFormComponent } from '../shared/user-form/user-form.component';

import { UserService } from '../../services/user/user.service';
import { RoleService } from '../../services/role/role.service';
import { BranchService } from '../../../branches/services/branch.service';

@Component({
  standalone: true,
  imports: [
    PageShellComponent,
    UserFormComponent
  ],
  template: `
  <app-page-shell>
    <div page-content>
      <app-user-form
        mode="create"
        [loading]="loading"
        [roles]="roles"
        [branches]="branches"
        (save)="submit($event)"
        (cancel)="cancel()">
      </app-user-form>
    </div>
  </app-page-shell>
`
})
export class UserCreateComponent {

  private api = inject(UserService);
  private rolesApi = inject(RoleService);
  private branchService = inject(BranchService);
  private router = inject(Router);
  private snack = inject(MatSnackBar);

  loading = false;

  roles: string[] = [];
  branches: any[] = [];

  constructor() {
    this.rolesApi.list().subscribe(r => {
      this.roles = r.map(x => x.name);
    });
    this.branchService.getAllLegacy().subscribe(v => this.branches = v);
  }

  private toFormData(payload: any): FormData {

    const fd = new FormData();

    fd.append(
      'username',
      payload.username || ''
    );

    fd.append(
      'password',
      payload.password || ''
    );

    fd.append(
      'role',
      payload.role || ''
    );

    if (payload.idNumber) {
      fd.append(
        'idNumber',
        payload.idNumber
      );
    }

    /* emailAddresses[] */
    (payload.emailAddresses || [])
      .forEach((v: string, i: number) => {
        fd.append(
          `emailAddresses[${i}]`,
          v
        );
      });

    /* phoneNumbers[] */
    (payload.phoneNumbers || [])
      .forEach((v: string, i: number) => {
        fd.append(
          `phoneNumbers[${i}]`,
          v
        );
      });

    (payload.userFiles || [])
      .forEach((f: any, i: number) => {

        fd.append(
          `userFiles[${i}].file`,
          f.file
        );

        fd.append(
          `userFiles[${i}].description`,
          f.description || ''
        );

      });

    (payload.branchIds || [])
      .forEach((id: string, i: number) => {
        fd.append(`branchIds[${i}]`, id);
      });

    if (payload.primaryBranchId) {
      fd.append(
        'primaryBranchId',
        payload.primaryBranchId
      );
    }

    return fd;

  }

  submit(payload: any) {

    this.loading = true;

    const fd = this.toFormData(payload);

    this.api.create(fd).subscribe({
      next: () => {
        this.loading = false;

        this.snack.open(
          'User created',
          'Close',
          { duration: 3000 }
        );

        this.router.navigate(
          ['/app/users']
        );
      },
      error: (err) => {
        this.loading = false;

        this.snack.open(
          err?.error?.message || 'User Creation failed. Contact system support or try a little bit later',
          'Close',
          { duration: 5000 }
        );
      }
    });

  }

  cancel() {
    this.router.navigate(['/app/users']);
  }
}