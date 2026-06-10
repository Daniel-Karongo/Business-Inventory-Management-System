import {
   SupplierMinimalDTO
} from '../../../suppliers/models/supplier.model';

export type StockWorkspaceRowType =
   | 'PRODUCT'
   | 'VARIANT'
   | 'INVENTORY';

export interface StockWorkspaceRow {
   /* =========================================================
      IDENTITY
   ========================================================= */
   id: string;
   type: StockWorkspaceRowType;

   parentId?: string | null;

   expanded?: boolean;
   hasChildren?: boolean;

   level: number;

   /* =========================================================
      PRODUCT
   ========================================================= */
   productId: string;
   productName: string;

   thumbnail?: string | null;

   categoryId?: number;
   categoryName?: string;

   deleted?: boolean;

   /* =========================================================
      VARIANT
   ========================================================= */
   variantId?: string;
   variantName?: string;

   sku?: string;
   barcode?: string;

   /* =========================================================
      BRANCH
   ========================================================= */
   branchId?: string;
   branchName?: string;

   /* =========================================================
      COMMERCIAL
   ========================================================= */
   suppliers?: SupplierMinimalDTO[];

   averageCost?: number;

   /**
    * FIFO next-sale cost
    */
   projectedNextSaleCost?: number;

   sellingPrice?: number;

   inventoryValue?: number;

   /**
    * Average cost margin
    */
   marginAmount?: number;
   marginPercent?: number;

   /**
    * FIFO projected margin
    */
   projectedMarginAmount?: number;
   projectedMarginPercent?: number;

   /* =========================================================
      INVENTORY
   ========================================================= */
   quantityOnHand?: number;
   quantityReserved?: number;
   quantityAvailable?: number;

   batchCount?: number;

   /* =========================================================
      AGGREGATES
   ========================================================= */
   totalVariants?: number;
   totalBranches?: number;

   /* =========================================================
      STATUS
   ========================================================= */
   lowStock?: boolean;
   outOfStock?: boolean;

   updatedAt?: string | null;

   childrenLoaded?: boolean;

   loadingChildren?: boolean;

   hidden?: boolean;
}