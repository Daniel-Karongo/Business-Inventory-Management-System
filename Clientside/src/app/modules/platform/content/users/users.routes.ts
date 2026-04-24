import { Routes } from '@angular/router';
import { platformModeGuard } from '../../../../core/guards/platform-mode.guard';
import { UserListComponent } from './pages/user-list/user-list.component';
import { UserCreateComponent } from './pages/user-create/user-create.component';

export const USER_ROUTES: Routes = [
    {
        path: '',
        component: UserListComponent,
        canActivate: [
            platformModeGuard
        ]
    },
    {
        path: 'create',
        component: UserCreateComponent,
        canActivate: [
            platformModeGuard
        ]
    },
    {
        path: ':id/edit',
        canActivate: [platformModeGuard],
        loadComponent: () => import(
            './pages/user-edit/user-edit.component'
        ).then(m => m.UserEditComponent)
    },

    {
        path: ':id',
        canActivate: [
            platformModeGuard
        ],
        children: [
            {
                path: '',
                loadComponent: () =>
                    import(
                        './pages/user-detail/user-detail.component'
                    ).then(
                        m => m.UserDetailComponent
                    )
            },
            {
                path: 'audit',
                loadComponent: () =>
                    import(
                        './pages/user-audit/user-audit.component'
                    ).then(
                        m => m.UserAuditComponent
                    )
            }
        ]
    }
];