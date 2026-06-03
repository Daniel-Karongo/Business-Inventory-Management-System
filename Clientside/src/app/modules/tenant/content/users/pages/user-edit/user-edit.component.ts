import { Component, inject } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { CommonModule } from '@angular/common';

import { MatSnackBar } from '@angular/material/snack-bar';
import { MatTabsModule } from '@angular/material/tabs';

import { PageShellComponent }
  from '../../../../../../shared/layout/page-shell/page-shell.component';

import { UserFormComponent }
  from '../shared/user-form/user-form.component';

import { EntityImageManagerComponent }
  from '../../../../../../shared/components/entity-image-manager/entity-image-manager.component';

import { UserService }
  from '../../services/user/user.service';

import { UserImageAdapter }
  from '../../services/user/user-image.adapter';

import { RoleService }
  from '../../services/role/role.service';

import { BranchService }
  from '../../../branches/services/branch.service';

import { AuthService }
  from '../../../../../auth/services/auth.service';


@Component({
  standalone: true,

  imports: [
    CommonModule,
    PageShellComponent,
    UserFormComponent,
    EntityImageManagerComponent,
    MatTabsModule
  ],

  template: `
<app-page-shell>

<div page-content>

<section class="edit-workspace">

<mat-tab-group
 class="user-tabs"
 animationDuration="150ms"
 dynamicHeight>

<mat-tab label="Profile">

<div class="tab-pane profile-pane">

<app-user-form
  [currentUserId]="currentUserId"
  mode="edit"
  [value]="user"
  [loading]="loading"
  [roles]="roles"
  [branches]="branches"
  (save)="save($event)"
  (cancel)="cancel()">
</app-user-form>

</div>

</mat-tab>


<mat-tab label="Documents">

<div class="tab-pane">

<div class="documents-panel">

<header class="documents-header">
<h3>Documents</h3>
<p>
Manage uploads, deleted files
and document lifecycle.
</p>
</header>

<app-entity-image-manager
*ngIf="user?.username"
[entityId]="user.username"
[adapter]="imageAdapter"
[allowHardDelete]="true">
</app-entity-image-manager>

</div>

</div>

</mat-tab>

</mat-tab-group>

</section>

</div>

</app-page-shell>
`,

  styles: [`

:host{
display:block;
}

[page-content]{
padding-bottom:40px;
}


/* one outer wrapper only */
.edit-workspace{
margin-top:8px;
}


/* soften tabs */
.user-tabs{
background:transparent;
}


/* remove ugly boxed divisions */
.tab-pane{
padding:20px 0 8px 0;
}


/* make form feel primary */
.profile-pane{
padding-top:8px;
}


/* documents section should feel related,
not like another independent page */
.documents-panel{
margin-top:10px;
padding:24px 26px;
border-radius:16px;
background:var(--surface);
border:1px solid var(--border);
box-shadow:0 2px 8px rgba(0,0,0,.03);
}


/* cleaner header */
.documents-header{
margin-bottom:18px;
}

.documents-header h3{
margin:0;
font-size:.95rem;
font-weight:600;
letter-spacing:.01em;
}

.documents-header p{
margin:6px 0 0;
font-size:.82rem;
color:var(--text-secondary);
line-height:1.45;
}


/* remove nested "card inside card" feel */
.documents-panel app-entity-image-manager{
display:block;
margin-top:14px;
}


/* Angular Material tab polish */
::ng-deep .mat-mdc-tab-header{
border-bottom:1px solid var(--border);
margin-bottom:8px;
}

::ng-deep .mat-mdc-tab{
min-width:120px;
}

::ng-deep .mdc-tab__text-label{
font-size:.88rem;
font-weight:500;
}


/* mobile */
@media(max-width:700px){

.tab-pane{
padding-top:14px;
}

.documents-panel{
padding:16px;
border-radius:12px;
}

}

`]

})
export class UserEditComponent {

  private api = inject(UserService);
  private rolesApi = inject(RoleService);
  private branchService = inject(BranchService);
  private auth = inject(AuthService);

  private route = inject(ActivatedRoute);
  private router = inject(Router);
  private snack = inject(MatSnackBar);

  imageAdapter = UserImageAdapter(this.api);

  id =
    this.route.snapshot.paramMap.get('username')!;

  loading = false;

  user: any;

  roles: string[] = [];
  branches: any[] = [];

  currentUserId = '';


  constructor() {

    this.auth.getCurrentUser()
      .subscribe(me => {
        this.currentUserId =
          me?.userId || '';
      });

    this.rolesApi.list()
      .subscribe(r => {
        this.roles =
          r.map(x => x.name);
      });

    this.branchService.getAllLegacy()
      .subscribe(v => {
        this.branches = v;
      });

    this.api.get(this.id)
      .subscribe(v => {
        this.user = v;
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

        const credentialChanged =
          payload.username !== this.user?.username ||
          !!payload.password?.trim();

        const message =
          credentialChanged
            ? 'User updated. All active sessions were revoked.'
            : 'User updated successfully';

        this.snack.open(
          message,
          'Close',
          { duration: 6000 }
        );

        const selfCredentialChange =
          this.currentUserId === this.user?.id &&
          (
            payload.username !== this.user?.username ||
            !!payload.password?.trim()
          );

        if (selfCredentialChange) {

          this.snack.open(
            'Credentials changed. You have been signed out.',
            'Close',
            { duration: 5000 }
          );

          this.auth.logout();

          this.router.navigate(
            ['/auth/login']
          );

          return;

        }

        this.router.navigate(
          ['/app/users', payload.username]
        );

      },

      error: (err) => {

        this.loading = false;

        this.snack.open(
          err?.error?.message ||
          err?.error?.detail ||
          'Update failed',
          'Close',
          { duration: 6000 }
        );

      }

    });

  }



  cancel() {

    this.router.navigate(
      ['/app/users', this.id]
    );

  }

}