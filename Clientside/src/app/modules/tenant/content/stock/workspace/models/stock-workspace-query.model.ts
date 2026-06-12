export interface StockWorkspaceQuery {
    page: number;
    size: number;

    filter?: StockWorkspaceFilterType;

    keyword?: string;

    branchId?: string;

    categoryId?: number;

    supplierId?: string;

    deleted?: boolean;

    sortBy?: string;

    direction?: 'asc' | 'desc';
}

export type StockWorkspaceFilterType =
    | 'ACTIVE'
    | 'DELETED'
    | 'ALL';