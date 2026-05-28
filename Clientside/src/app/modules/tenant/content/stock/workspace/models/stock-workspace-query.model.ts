export interface StockWorkspaceQuery {
    page: number;
    size: number;

    keyword?: string;

    branchId?: string;

    categoryId?: number;

    supplierId?: string;

    deleted?: boolean;

    sortBy?: string;

    direction?: 'asc' | 'desc';
}