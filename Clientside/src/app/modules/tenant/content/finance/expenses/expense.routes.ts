import {
    Routes
} from '@angular/router';

import {
    roleAtLeast
} from '../../../../../core/security/role-at-least.guard';

export const EXPENSE_ROUTES: Routes = [
    {
        path: '',
        canMatch: [
            roleAtLeast(
                'MANAGER'
            )
        ],
        children: [
            {
                path: '',
                loadComponent: () =>
                    import(
                        './pages/expense-list/expense-list.component'
                    ).then(
                        m =>
                            m.OperationalExpenseListComponent
                    )
            },
            {
                path: ':expenseId',
                loadComponent: () =>
                    import(
                        './pages/expense-workspace/expense-workspace.component'
                    ).then(
                        m =>
                            m.ExpenseWorkspaceComponent
                    )
            }
        ]
    }
];