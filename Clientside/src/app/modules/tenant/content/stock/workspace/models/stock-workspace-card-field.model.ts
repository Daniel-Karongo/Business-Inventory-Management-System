export interface StockWorkspaceCardField<T> {
    label: string;
    value: (row: T) => unknown;
}