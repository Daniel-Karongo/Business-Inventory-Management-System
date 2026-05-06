import { TransactionType } from './transaction-type.model';

export interface StockTransactionDTO {
  id: string;

  productId: string;

  productName: string;

  productVariantId: string;

  productVariantName: string;

  branchId: string;

  branchName: string;

  type: TransactionType;

  quantityDelta: number;

  unitCost?: number;

  reference?: string;

  supplierId?: string;

  note?: string;

  timestamp: string;

  performedBy?: string;
}