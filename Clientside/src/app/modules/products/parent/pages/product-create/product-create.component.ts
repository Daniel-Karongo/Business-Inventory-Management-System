import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormBuilder, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms';
import { Router } from '@angular/router';

import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import { ProductService } from '../../services/product.service';
import { CategoryService } from '../../../../categories/services/category.service';
import { SupplierService } from '../../../../suppliers/services/supplier.service';
import { Category } from '../../../../categories/models/category.model';
import { Supplier } from '../../../../suppliers/models/supplier.model';

@Component({
  selector: 'app-product-create',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatButtonModule,
    MatIconModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatSnackBarModule
  ],
  templateUrl: './product-create.component.html',
  styleUrls: ['./product-create.component.scss']
})
export class ProductCreateComponent implements OnInit {

  form!: FormGroup;
  saving = false;

  categories: Category[] = [];
  suppliers: Supplier[] = [];

  constructor(
    private fb: FormBuilder,
    private productService: ProductService,
    private categoryService: CategoryService,
    private supplierService: SupplierService,
    private snackbar: MatSnackBar,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.buildForm();
    this.loadLookups();
  }

  private buildForm() {
    this.form = this.fb.group({
      name: ['', Validators.required],
      description: [''],
      categoryId: [null, Validators.required],
      supplierIds: [[]],
      minimumPercentageProfit: [null, [Validators.min(0)]]
    });
  }

  private loadLookups() {
    this.categoryService
      .getAll('tree', false)
      .subscribe(c => this.categories = c || []);

    this.supplierService
      .getAll(false)
      .subscribe(s => this.suppliers = s || []);
  }

  save() {
    if (this.form.invalid || this.saving) return;

    this.saving = true;

    const payload = this.form.value;

    this.productService.create(this.toFormData(payload)).subscribe({
      next: product => {
        this.snackbar.open('Product created', 'Close', { duration: 2000 });
        this.router.navigate(['/products', product.id]);
      },
      error: () => {
        this.saving = false;
        this.snackbar.open('Create failed', 'Close', { duration: 3000 });
      }
    });
  }

  cancel() {
    this.router.navigate(['/products']);
  }

  /**
   * Backend expects multipart/form-data
   */
  private toFormData(data: any): FormData {
    const fd = new FormData();

    fd.append('name', data.name);
    if (data.description) fd.append('description', data.description);
    fd.append('categoryId', data.categoryId);
    fd.append('minimumPercentageProfit', data.minimumPercentageProfit ?? '');

    (data.supplierIds || []).forEach((id: string) =>
      fd.append('supplierIds', id)
    );

    return fd;
  }
}