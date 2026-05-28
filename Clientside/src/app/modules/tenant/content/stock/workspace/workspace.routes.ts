import { Routes }
    from '@angular/router';

export const STOCK_WORKSPACE_ROUTES:
    Routes = [
        {
            path: '',
            loadComponent: () =>
                import(
                    './pages/stock-workspace/stock-workspace.component'
                ).then(
                    m => m.StockWorkspaceComponent
                )
        }
    ];