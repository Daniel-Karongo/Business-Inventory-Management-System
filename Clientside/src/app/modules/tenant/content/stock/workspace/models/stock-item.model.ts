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

   productSku?: string;

   thumbnailFileName?: string;

   primaryImageFileName?: string;

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

   /**
    * undefined = inventory not checked yet
    * true      = inventory exists
    * false     = inventory checked and none exists
    */
   hasInventory?: boolean;

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

   projectedNextSaleCost?: number;

   sellingPrice?: number;

   inventoryValue?: number;

   marginAmount?: number;
   marginPercent?: number;

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