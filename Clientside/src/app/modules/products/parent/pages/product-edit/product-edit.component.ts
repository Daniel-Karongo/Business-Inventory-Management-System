import { Component, OnInit } from '@angular/core';
import { CommonModule, Location } from '@angular/common';
import { ActivatedRoute, Router } from '@angular/router';
import { ReactiveFormsModule, FormBuilder, Validators, FormGroup } from '@angular/forms';

import { MatButtonModule } from '@angular/material/button';
import { MatSelectModule } from '@angular/material/select';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';

import { ProductService } from '../../services/product.service';
import { CategoryService } from '../../../../categories/services/category.service';
import { SupplierService } from '../../../../suppliers/services/supplier.service';

@Component({
  selector: 'app-product-edit',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatButtonModule,
    MatSelectModule,
    MatFormFieldModule,
    MatInputModule,
    MatSnackBarModule,
    MatDialogModule
  ],
  templateUrl: './product-edit.component.html',
  styleUrls: ['./product-edit.component.scss']
})
export class ProductEditComponent implements OnInit {

  productId!: string;
  product: any;
  loading = true;
  saving = false;

  categories: any[] = [];
  suppliers: any[] = [];

  originalCategoryId?: number;

  form!: FormGroup;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private fb: FormBuilder,
    private productService: ProductService,
    private categoryService: CategoryService,
    private supplierService: SupplierService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {
    const id = this.route.snapshot.paramMap.get('id');
    if (!id) {
      this.router.navigate(['/products']);
      return;
    }

    this.form = this.fb.group({
      name: ['', Validators.required],
      description: [''],
      categoryId: [null, Validators.required],
      supplierIds: [[] as string[]],
      minimumPercentageProfit: [null]
    });

    this.productId = id;

    this.loadLookups();
    this.loadProduct();
  }

  private loadLookups() {
    this.categoryService.getAll("flat", false).subscribe(c => this.categories = c || []);
    this.supplierService.getAll(false).subscribe(s => this.suppliers = s || []);
  }

  private loadProduct() {
    const deleted = history.state?.deleted === true;

    this.productService.getById(this.productId, deleted).subscribe({
      next: p => {
        this.product = p;
        this.originalCategoryId = p.categoryId;

        this.form.patchValue({
          name: p.name,
          description: p.description,
          categoryId: p.categoryId,
          supplierIds: p.suppliers?.map((s: any) => s.id) || [],
          minimumPercentageProfit: p.minimumPercentageProfit
        });

        if (p.deleted) {
          this.form.disable();
        }

        this.loading = false;
      },
      error: () => {
        this.router.navigate(['/products']);
      }
    });
  }

  save() {
    if (this.form.invalid || this.saving || this.product.deleted) return;

    const payload = this.form.value;

    const categoryChanged =
      payload.categoryId !== this.originalCategoryId;

    const proceed = () => {
      this.saving = true;

      this.productService.update(this.productId, payload).subscribe({
        next: () => {
          this.snackbar.open('Product updated', 'Close', { duration: 2000 });
          this.router.navigate(['/products', this.productId]);
        },
        error: () => {
          this.saving = false;
          this.snackbar.open('Update failed', 'Close', { duration: 3000 });
        }
      });
    };

    if (categoryChanged) {
      const ok = confirm(
        'Changing category may affect SKU structure and reporting. Continue?'
      );
      if (!ok) return;
    }

    proceed();
  }

  cancel() {
    this.router.navigate(['/products', this.productId]);
  }
}