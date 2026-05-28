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
   sellingPrice?: number;

   inventoryValue?: number;

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

   capabilities?: {

      receivable?: boolean;

      adjustable?: boolean;

      transferable?: boolean;

      sellable?: boolean;

      editable?: boolean;

      deletable?: boolean;

      restorable?: boolean;

      inspectable?: boolean;
   };

   childrenLoaded?: boolean;

   loadingChildren?: boolean;

   hidden?: boolean;
}