import { Component, OnInit } from '@angular/core';
import { CommonModule, Location } from '@angular/common';
import { ActivatedRoute, Router } from '@angular/router';

import { MatCardModule } from '@angular/material/card';
import { MatTabsModule } from '@angular/material/tabs';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar } from '@angular/material/snack-bar';

import { SupplierService } from '../../services/supplier.service';
import { Supplier, SupplierAudit } from '../../models/supplier.model';

import { AuthService } from '../../../auth/services/auth.service';
import { SupplierImageAdapter } from '../../services/supplier-image.adapter';
import { EntityImageManagerComponent } from
  '../../../../shared/components/entity-image-manager/entity-image-manager.component';

import { Product } from '../../../products/parent/models/product.model';
import { CategoryService } from '../../../categories/services/category.service';
import { Category } from '../../../categories/models/category.model';

import {
  EntityActionService,
  EntityActionConfig
} from '../../../../shared/services/entity-action.service';

import {
  SUPPLIER_ACTION_REASONS,
  SupplierActionType
} from '../../models/supplier-action-reasons.model';

@Component({
  selector: 'app-supplier-details',
  standalone: true,
  imports: [
    CommonModule,
    MatCardModule,
    MatTabsModule,
    MatButtonModule,
    MatIconModule,
    EntityImageManagerComponent
  ],
  templateUrl: './supplier-details.component.html',
  styleUrls: ['./supplier-details.component.scss']
})
export class SupplierDetailsComponent implements OnInit {

  supplier!: Supplier;
  supplierId!: string;
  loading = true;
  role = '';

  products: Product[] = [];
  audits: SupplierAudit[] = [];
  categories: CategoryWithSupply[] = [];

  imageAdapter!: any;
  private actionConfig!: EntityActionConfig<Supplier>;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private location: Location,
    private supplierService: SupplierService,
    private categoryService: CategoryService,
    private entityAction: EntityActionService,
    private auth: AuthService,
    private snackbar: MatSnackBar
  ) { }

  ngOnInit(): void {

    this.auth.getCurrentUser().subscribe(u => {
      this.role = u?.role ?? '';
    });

    this.imageAdapter = SupplierImageAdapter(this.supplierService);

    const identifier = this.route.snapshot.paramMap.get('identifier');
    if (!identifier) return;

    const nav = this.router.getCurrentNavigation();
    const deleted = nav?.extras.state?.['deleted'] ?? false;

    this.loadSupplier(identifier, deleted);
    this.loadCategories();
  }

  /* ================= LOAD ================= */

  private loadSupplier(identifier: string, deleted: boolean) {

    this.loading = true;

    this.supplierService.getById(identifier, deleted).subscribe({
      next: s => {
        this.supplier = s;
        this.supplierId = s.id;
        this.loading = false;

        this.initializeActionConfig();
        this.loadProducts();
        this.loadAudits();
      },
      error: () => {
        this.snackbar.open('Failed to load supplier', 'Close', { duration: 3000 });
        this.router.navigate(['/suppliers']);
      }
    });
  }

  private initializeActionConfig() {

    this.actionConfig = {
      entityName: 'Supplier',
      displayName: (s) => s.name,

      disableReasons: SUPPLIER_ACTION_REASONS[SupplierActionType.DISABLE],
      restoreReasons: SUPPLIER_ACTION_REASONS[SupplierActionType.RESTORE],
      deleteReasons: SUPPLIER_ACTION_REASONS[SupplierActionType.DELETE],

      disable: (id, reason) =>
        this.supplierService.softDelete(id, reason),

      restore: (id, reason) =>
        this.supplierService.restore(id, reason),

      hardDelete: (id, reason) =>
        this.supplierService.hardDelete(id, reason),

      reload: () =>
        this.loadSupplier(this.supplierId, this.supplier.deleted)
    };
  }

  private loadProducts() {
    this.supplierService.productsSupplied(this.supplierId)
      .subscribe(products => this.products = products || []);
  }

  private loadAudits() {
    this.supplierService
      .supplierAudits(this.supplierId)
      .subscribe(a => this.audits = a || []);
  }

  loadCategories() {
    this.categoryService.getAll('tree', false).subscribe(tree => {
      this.categories = tree.map(c =>
        computeCategorySupplyState(c, this.supplierId)
      );
    });
  }

  /* ================= ROLE GUARDS ================= */

  canManageSupplier(): boolean {
    return ['ADMIN', 'SUPERUSER'].includes(this.role);
  }

  isSuperuser(): boolean {
    return this.role === 'SUPERUSER';
  }

  /* ================= ACTION ================= */

  toggleSupplier() {
    this.entityAction.toggleSingle(this.supplier, this.actionConfig);
  }

  back() {
    this.location.back();
  }

  editSupplier() {
    this.router.navigate(
      ['/suppliers', this.supplierId, 'edit'],
      { state: { deleted: this.supplier.deleted } }
    );
  }

  goProduct(id: string) {
    this.router.navigate(['/products', id]);
  }

  status(): string {
    return this.supplier.deleted ? 'DISABLED' : 'ACTIVE';
  }
}

/* ===========================================================
   CATEGORY SUPPLY TYPES + UTIL (RESTORED)
=========================================================== */

export type CategorySupplyState =
  | 'DIRECT'
  | 'DERIVED'
  | 'PARTIAL'
  | 'NONE';

export interface CategoryWithSupply extends Category {
  supplyState: CategorySupplyState;
  suppliedChildrenCount: number;
  totalChildrenCount: number;
}

export function computeCategorySupplyState(
  category: Category,
  supplierId: string
): CategoryWithSupply {

  const children = category.subcategories ?? [];

  const isDirect =
    category.suppliers?.some(s => s.id === supplierId) ?? false;

  if (isDirect) {
    return {
      ...category,
      supplyState: 'DIRECT',
      suppliedChildrenCount: children.length,
      totalChildrenCount: children.length,
      subcategories: children.map(c =>
        computeCategorySupplyState(c, supplierId)
      )
    };
  }

  if (children.length === 0) {
    return {
      ...category,
      supplyState: 'NONE',
      suppliedChildrenCount: 0,
      totalChildrenCount: 0
    };
  }

  const computedChildren = children.map(c =>
    computeCategorySupplyState(c, supplierId)
  );

  const directCount = computedChildren.filter(
    c => c.supplyState === 'DIRECT'
  ).length;

  if (directCount === computedChildren.length) {
    return {
      ...category,
      supplyState: 'DERIVED',
      suppliedChildrenCount: directCount,
      totalChildrenCount: computedChildren.length,
      subcategories: computedChildren
    };
  }

  if (directCount > 0) {
    return {
      ...category,
      supplyState: 'PARTIAL',
      suppliedChildrenCount: directCount,
      totalChildrenCount: computedChildren.length,
      subcategories: computedChildren
    };
  }

  return {
    ...category,
    supplyState: 'NONE',
    suppliedChildrenCount: 0,
    totalChildrenCount: computedChildren.length,
    subcategories: computedChildren
  };
}