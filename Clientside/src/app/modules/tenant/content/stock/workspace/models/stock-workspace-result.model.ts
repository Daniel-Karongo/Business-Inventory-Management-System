import {
    StockWorkspaceRow
} from './stock-item.model';

export interface StockWorkspaceResult {

    content:
    StockWorkspaceRow[];

    totalElements:
    number;

    page:
    number;

    size:
    number;

}