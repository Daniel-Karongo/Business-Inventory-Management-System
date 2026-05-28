export interface StockWorkspaceAction<T> {
    icon: string;
    tooltip: string;
    hidden?: (row: T) => boolean;
    disabled?: (row: T) => boolean;
    execute: (row: T) => void;
}