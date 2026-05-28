export interface StockWorkspaceColumn<T> {
    id: string;
    label: string;
    sortable?: boolean;
    hiddenMobile?: boolean;
    value: (row: T) => unknown;
}