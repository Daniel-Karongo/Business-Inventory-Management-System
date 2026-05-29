import { Routes } from '@angular/router';
import { roleAtLeast }
    from '../../../../../core/security/role-at-least.guard';

export const STOCK_WORKSPACE_ROUTES: Routes = [

    /* ============================================================
       WORKSPACE
    ============================================================ */

    {
        path: '',
        canMatch: [
            roleAtLeast('EMPLOYEE')
        ],
        loadComponent: () =>
            import(
                './pages/stock-workspace/stock-workspace.component'
            ).then(
                m => m.StockWorkspaceComponent
            )
    },

    /* ============================================================
       PRODUCT CREATE
    ============================================================ */

    {
        path: 'create',
        canMatch: [
            roleAtLeast('EMPLOYEE')
        ],
        loadComponent: () =>
            import(
                '../products/parent/pages/product-create/product-create.component'
            ).then(
                m => m.ProductCreateComponent
            )
    },

    /* ============================================================
       INVENTORY TRANSACTIONS
       MUST BE BEFORE :id
    ============================================================ */

    {
        path: 'inventory/:variantId',
        canMatch: [
            roleAtLeast('EMPLOYEE')
        ],
        loadComponent: () =>
            import(
                '../inventory/pages/inventory-transactions/inventory-transactions.component'
            ).then(
                m => m.InventoryTransactionsComponent
            )
    },

    /* ============================================================
       PRODUCT EDIT
       MUST BE BEFORE :id
    ============================================================ */

    {
        path: ':id/edit',
        canMatch: [
            roleAtLeast('EMPLOYEE')
        ],
        loadComponent: () =>
            import(
                '../products/parent/pages/product-edit/product-edit.component'
            ).then(
                m => m.ProductEditComponent
            )
    },

    /* ============================================================
       PRODUCT DETAILS
    ============================================================ */

    {
        path: ':id',
        canMatch: [
            roleAtLeast('EMPLOYEE')
        ],
        loadComponent: () =>
            import(
                '../products/parent/pages/product-details/product-details.component'
            ).then(
                m => m.ProductDetailsComponent
            )
    }
];