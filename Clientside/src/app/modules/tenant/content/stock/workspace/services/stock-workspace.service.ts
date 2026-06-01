import {
  Injectable,
  inject
} from '@angular/core';

import {
  BehaviorSubject,
  Observable,
  forkJoin,
  map,
  of,
  switchMap,
  tap
} from 'rxjs';

import {
  ProductService
} from '../../products/parent/services/product.service';

import {
  ProductVariantService
} from '../../products/variant/services/product-variant.service';

import {
  InventoryService
} from '../../inventory/services/inventory.service';

import {
  Product
} from '../../models/product.model';

import {
  ProductVariant
} from '../../models/product-variant.model';

import {
  InventoryResponse,
  InventoryWorkspaceResponse
} from '../../models/inventory-response.model';

import {
  StockWorkspaceQuery
} from '../models/stock-workspace-query.model';

import {
  StockWorkspaceResult
} from '../models/stock-workspace-result.model';

import {
  StockWorkspaceRow
} from '../models/stock-item.model';
import { MatSnackBar } from '@angular/material/snack-bar';

@Injectable({
  providedIn: 'root'
})
export class StockWorkspaceService {

  private productService =
    inject(ProductService);

  private variantService =
    inject(ProductVariantService);

  private inventoryService =
    inject(InventoryService);

  private snackBar =
    inject(MatSnackBar);

  /* =========================================================
     STATE
  ========================================================= */

  private branchMap =
    new Map<string, string>();

  private rows =
    new Map<string, StockWorkspaceRow>();

  private rootIds: string[] = [];

  private expandedRows =
    new Set<string>();

  private activeBranchId?:
    string;

  /* =========================================================
     SEARCH
  ========================================================= */

  search(
    query: StockWorkspaceQuery
  ): Observable<StockWorkspaceResult> {

    if (!query.branchId) {
      throw new Error(
        'Branch is required'
      );
    }

    this.activeBranchId =
      query.branchId;

    return this.productService.search({
      branchId: query.branchId,
      page: query.page,
      size: query.size,
      keyword: query.keyword,
      categoryId: query.categoryId,
      supplierId: query.supplierId,
      deleted: query.deleted,
      sortBy: query.sortBy,
      direction: query.direction
    })
      .pipe(
        map(page => {

          if (!page) {
            return {
              content: [],
              totalElements: 0,
              page: query.page,
              size: query.size
            };
          }

          this.rows.clear();
          this.rootIds = [];
          this.expandedRows.clear();

          const products =
            page.content ?? [];

          for (
            const product
            of products
          ) {

            const row =
              this.buildProductRow(
                product
              );

            this.rows.set(
              row.id,
              row
            );

            this.rootIds.push(
              row.id
            );
          }

          return {
            content:
              this.getVisibleRows(),
            totalElements:
              page.totalElements,
            page:
              page.pageNumber ?? query.page,
            size:
              page.pageSize ?? query.size
          };
        })
      );
  }

  /* =========================================================
     EXPANSION
  ========================================================= */

  toggleExpanded(
    row: StockWorkspaceRow
  ): Observable<void> {

    if (
      this.expandedRows.has(
        row.id
      )
    ) {

      this.expandedRows.delete(
        row.id
      );

      return of(void 0);
    }

    this.expandedRows.add(
      row.id
    );

    if (
      row.childrenLoaded
    ) {

      if (
        row.type === 'PRODUCT' &&
        !row.hasChildren
      ) {

        this.snackBar.open(
          'No variants exist for this product in the selected branch.',
          'Close',
          {
            duration: 4000
          }
        );

      }

      return of(void 0);
    }

    row.loadingChildren = true;

    if (
      row.type === 'PRODUCT'
    ) {

      return this.loadVariants(
        row
      ).pipe(
        tap(() => {

          if (
            row.childrenLoaded &&
            !row.hasChildren
          ) {

            this.snackBar.open(
              'No variants exist for this product in the selected branch.',
              'Close',
              {
                duration: 4000
              }
            );

          }

        })
      );
    }

    if (
      row.type === 'VARIANT'
    ) {

      return this.loadInventory(
        row
      ).pipe(
        tap(() => {

          if (
            row.childrenLoaded &&
            !row.hasChildren
          ) {

            this.snackBar.open(
              'No inventory exists for this variant in the selected branch.',
              'Close',
              {
                duration: 4000
              }
            );

          }

        })
      );
    }

    return of(void 0);
  }

  /* =========================================================
     LOAD VARIANTS
  ========================================================= */

  private loadVariants(
    productRow: StockWorkspaceRow
  ): Observable<void> {

    return this.variantService
      .forProduct(
        productRow.productId,
        'ACTIVE',
        this.activeBranchId
      )
      .pipe(
        tap(variants => {

          for (
            const variant
            of variants
          ) {

            const row =
              this.buildVariantRow(
                {
                  id:
                    productRow.productId,
                  name:
                    productRow.productName,
                  deleted:
                    !!productRow.deleted,
                  updatedAt:
                    productRow.updatedAt,
                  variants: [],
                  imageUrls: [],
                  suppliers:
                    productRow.suppliers ?? []
                } as Product,
                variant,
                productRow.id
              );

            this.rows.set(
              row.id,
              row
            );
          }

          productRow.childrenLoaded =
            true;

          productRow.loadingChildren =
            false;

          if (variants.length === 0) {

            productRow.hasChildren = false;

            console.warn(
              `No variants found for product ${productRow.productId} in branch ${this.activeBranchId}`
            );

          } else {

            productRow.hasChildren = true;

          }
        }),
        map(() => void 0)
      );
  }

  /* =========================================================
     LOAD INVENTORY
  ========================================================= */

  private loadInventory(
    variantRow: StockWorkspaceRow
  ): Observable<void> {

    if (
      !variantRow.variantId ||
      !this.activeBranchId
    ) {
      variantRow.loadingChildren = false;
      variantRow.childrenLoaded = true;
      variantRow.hasChildren = false;
      return of(void 0);
    }

    const inventoryRowId =
      `inventory_${variantRow.variantId}_${this.activeBranchId}`;

    // Remove any stale inventory row previously attached
    this.rows.delete(inventoryRowId);

    return this.inventoryService
      .getVariantStock(
        variantRow.variantId,
        this.activeBranchId
      )
      .pipe(
        tap(inventory => {

          if (inventory) {

            const row =
              this.buildInventoryRow(
                variantRow,
                inventory
              );

            this.rows.set(
              row.id,
              row
            );

            variantRow.hasChildren = true;

          } else {

            variantRow.hasChildren = false;

          }

          variantRow.childrenLoaded = true;
          variantRow.loadingChildren = false;
        }),
        map(() => void 0)
      );
  }

  /* =========================================================
     PRODUCT ROW
  ========================================================= */

  private buildProductRow(
    product: Product
  ): StockWorkspaceRow {

    return {
      id:
        `product_${product.id}`,

      type:
        'PRODUCT',

      level: 0,

      expanded: false,

      hasChildren:
        true,

      childrenLoaded:
        false,

      loadingChildren:
        false,

      productId:
        product.id,

      productName:
        product.name,

      thumbnail:
        product.thumbnail,

      categoryId:
        product.categoryId,

      categoryName:
        product.categoryName,

      suppliers:
        product.suppliers,

      totalVariants:
        product.variants?.length ?? 0,

      deleted:
        product.deleted,

      updatedAt:
        product.updatedAt
    };
  }

  /* =========================================================
     VARIANT ROW
  ========================================================= */

  private buildVariantRow(
    product: Product,
    variant: ProductVariant,
    parentId: string
  ): StockWorkspaceRow {

    return {
      id:
        `variant_${variant.id}`,

      parentId,

      type:
        'VARIANT',

      level: 1,

      expanded: false,

      hasChildren:
        true,

      childrenLoaded:
        false,

      loadingChildren:
        false,

      productId:
        product.id,

      productName:
        product.name,

      variantId:
        variant.id,

      variantName:
        variant.classification,

      sku:
        variant.sku,

      barcode:
        variant.barcode,

      deleted:
        product.deleted,

      updatedAt:
        product.updatedAt
    };
  }

  /* =========================================================
     INVENTORY ROW
  ========================================================= */

  private buildInventoryRow(
    variantRow: StockWorkspaceRow,
    inventory: InventoryWorkspaceResponse
  ): StockWorkspaceRow {

    return {
      id:
        `inventory_${variantRow.variantId}_${inventory.branchId}`,

      parentId:
        variantRow.id,

      type: 'INVENTORY',

      level: 2,

      expanded: false,
      hasChildren: false,

      productId:
        variantRow.productId,

      productName:
        variantRow.productName,

      variantId:
        variantRow.variantId,

      variantName:
        variantRow.variantName,

      sku:
        inventory.productVariantSku,

      barcode:
        variantRow.barcode,

      branchId:
        inventory.branchId,

      branchName:
        this.resolveBranchName(
          inventory.branchId
        ),

      quantityOnHand:
        inventory.quantityOnHand,

      quantityReserved:
        inventory.quantityReserved,

      quantityAvailable:
        inventory.quantityAvailable,

      averageCost:
        inventory.averageCost,

      inventoryValue:
        inventory.inventoryValue,

      batchCount:
        inventory.batchCount,

      updatedAt:
        inventory.updatedAt,
    };
  }

  /* =========================================================
     VISIBLE ROWS
  ========================================================= */

  getVisibleRows():
    StockWorkspaceRow[] {

    const visible:
      StockWorkspaceRow[] = [];

    const appendChildren = (
      parentId: string
    ) => {

      const children =
        [
          ...this.rows.values()
        ]
          .filter(
            x =>
              x.parentId === parentId
          );

      for (
        const child
        of children
      ) {

        child.expanded =
          this.expandedRows.has(
            child.id
          );

        visible.push(
          child
        );

        if (
          child.expanded
        ) {
          appendChildren(
            child.id
          );
        }
      }
    };

    for (
      const rootId
      of this.rootIds
    ) {

      const row =
        this.rows.get(
          rootId
        );

      if (!row) {
        continue;
      }

      row.expanded =
        this.expandedRows.has(
          row.id
        );

      visible.push(
        row
      );

      if (
        row.expanded
      ) {
        appendChildren(
          row.id
        );
      }
    }

    return visible;
  }

  /* =========================================================
     BRANCHES
  ========================================================= */

  setBranches(
    branches: {
      id: string;
      name: string;
    }[]
  ) {

    this.branchMap.clear();

    for (
      const branch
      of branches
    ) {
      this.branchMap.set(
        branch.id,
        branch.name
      );
    }
  }

  private resolveBranchName(
    branchId?: string
  ): string {

    if (!branchId) {
      return 'Unknown Branch';
    }

    return (
      this.branchMap.get(
        branchId
      ) ?? 'Unknown Branch'
    );
  }
}