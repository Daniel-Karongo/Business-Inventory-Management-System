import { Component, OnInit } from '@angular/core';
import { CommonModule, Location } from '@angular/common';
import { ActivatedRoute, Router } from '@angular/router';

import { MatCardModule } from '@angular/material/card';
import { MatTabsModule } from '@angular/material/tabs';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar } from '@angular/material/snack-bar';

import { SupplierService } from '../../services/supplier.service';
import {
  Supplier,
  SupplierAudit
} from '../../models/supplier.model';

import { AuthService } from '../../../auth/services/auth.service';
import { SupplierImageAdapter } from '../../services/supplier-image.adapter';
import { EntityImageManagerComponent } from
  '../../../../shared/components/entity-image-manager/entity-image-manager.component';

import { Product } from '../../../products/parent/models/product.model';
import { CategoryService } from '../../../categories/services/category.service';
import { Category } from '../../../categories/models/category.model';

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
  derivedCategories: { id: number; name: string }[] = [];
  audits: SupplierAudit[] = [];

  imageAdapter!: any;
  categories: CategoryWithSupply[] = [];

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private location: Location,
    private supplierService: SupplierService,
    private categoryService: CategoryService,
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

        this.loadProducts();
        this.loadAudits();
      },
      error: () => {
        this.snackbar.open('Failed to load supplier', 'Close', { duration: 3000 });
        this.router.navigate(['/suppliers']);
      }
    });
  }

  private loadProducts() {
    this.supplierService.productsSupplied(this.supplierId).subscribe(products => {
      this.products = products || [];
      this.computeCategoriesFromProducts();
    });
  }

  private computeCategoriesFromProducts() {
    const map = new Map<number, string>();

    this.products.forEach(p => {
      if (p.categoryId && p.categoryName) {
        map.set(p.categoryId, p.categoryName);
      }
    });

    this.derivedCategories = Array.from(map.entries()).map(
      ([id, name]) => ({ id, name })
    );
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

  isSupplied(category: Category): boolean {
    return !!category.suppliers?.some(s => s.id === this.supplierId);
  }

  hasSuppliedDescendant(category: Category): boolean {
    if (!category.subcategories?.length) return false;

    return category.subcategories.some(
      c => this.isSupplied(c) || this.hasSuppliedDescendant(c)
    );
  }

  /* ================= PERMISSIONS ================= */

  isSuperuser() {
    return this.role === 'SUPERUSER';
  }

  canSoftDelete() {
    return ['ADMIN', 'SUPERUSER'].includes(this.role);
  }

  canHardDelete() {
    return this.role === 'SUPERUSER';
  }

  /* ================= ACTIONS ================= */

  back() {
    this.location.back();
  }

  editSupplier() {
    this.router.navigate(
      ['/suppliers', this.supplierId, 'edit'],
      { state: { deleted: this.supplier.deleted } }
    );
  }

  disableSupplier() {
    this.supplierService
      .softDelete(this.supplierId, 'Disabled from supplier details')
      .subscribe(() => {
        this.supplier.deleted = true;
        this.snackbar.open('Supplier disabled', 'Close', { duration: 2000 });
      });
  }

  restoreSupplier() {
    this.supplierService
      .restore(this.supplierId, 'Restored from supplier details')
      .subscribe(() => {
        this.supplier.deleted = false;
        this.snackbar.open('Supplier restored', 'Close', { duration: 2000 });
      });
  }

  hardDeleteSupplier() {
    this.supplierService.hardDelete(this.supplierId).subscribe(() => {
      this.snackbar.open('Supplier permanently deleted', 'Close', { duration: 2000 });
      this.router.navigate(['/suppliers']);
    });
  }

  goProduct(id: string) {
    this.router.navigate(['/products', id]);
  }

  status(): string {
    return this.supplier.deleted ? 'DISABLED' : 'ACTIVE';
  }
}

// category-supply.model.ts
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

// category-supply.util.ts
export function computeCategorySupplyState(
  category: Category,
  supplierId: string
): CategoryWithSupply {

  const children = category.subcategories ?? [];

  const isDirect =
    category.suppliers?.some(s => s.id === supplierId) ?? false;

  // 1️⃣ DIRECT
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

  // 2️⃣ LEAF
  if (children.length === 0) {
    return {
      ...category,
      supplyState: 'NONE',
      suppliedChildrenCount: 0,
      totalChildrenCount: 0
    };
  }

  // 3️⃣ RECURSE
  const computedChildren = children.map(c =>
    computeCategorySupplyState(c, supplierId)
  );

  const directCount = computedChildren.filter(
    c => c.supplyState === 'DIRECT'
  ).length;

  // 4️⃣ DERIVED = ALL children DIRECT
  if (directCount === computedChildren.length) {
    return {
      ...category,
      supplyState: 'DERIVED',
      suppliedChildrenCount: directCount,
      totalChildrenCount: computedChildren.length,
      subcategories: computedChildren
    };
  }

  // 5️⃣ PARTIAL = SOME children DIRECT
  if (directCount > 0) {
    return {
      ...category,
      supplyState: 'PARTIAL',
      suppliedChildrenCount: directCount,
      totalChildrenCount: computedChildren.length,
      subcategories: computedChildren
    };
  }

  // 6️⃣ NONE
  return {
    ...category,
    supplyState: 'NONE',
    suppliedChildrenCount: 0,
    totalChildrenCount: computedChildren.length,
    subcategories: computedChildren
  };
}