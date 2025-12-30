export interface StockTransactionDTO {
  id: string;

  productId: string;
  productName: string;
  productVariantId: string;
  productVariantName: string;
  branchId: string;
  branchName: string;

  type:
    | 'RECEIPT'
    | 'SALE'
    | 'ADJUSTMENT'
    | 'TRANSFER_IN'
    | 'TRANSFER_OUT'
    | 'RESERVATION'
    | 'RELEASE'
    | 'RETURN';

  quantityDelta: number;
  unitCost?: number;

  reference?: string;
  supplierId?: string;

  note?: string;
  timestamp: string;
  performedBy?: string;
}