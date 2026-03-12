import { CommonModule } from '@angular/common';
import {
  Component,
  EventEmitter,
  Input,
  OnInit,
  Output,
  OnDestroy
} from '@angular/core';
import {
  FormBuilder,
  FormGroup,
  ReactiveFormsModule,
  Validators
} from '@angular/forms';
import {
  MatFormFieldModule
} from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';

import { Subject, forkJoin, takeUntil } from 'rxjs';

import { Category } from '../../models/category.model';
import { CategoryService } from '../../services/category.service';
import { SupplierService } from '../../../suppliers/services/supplier.service';

@Component({
  selector: 'app-category-form',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatButtonModule,
    MatProgressSpinnerModule
  ],
  templateUrl: './category-form.component.html',
  styleUrls: ['./category-form.component.scss']
})
export class CategoryFormComponent implements OnInit, OnDestroy {

  @Input() initialData: Category | null = null;
  @Output() submitted = new EventEmitter<{
    payload: any,
    done: () => void
  }>();

  private destroy$ = new Subject<void>();

  form!: FormGroup;

  loading = true;
  submitting = false;
  error: string | null = null;

  categories: Category[] = [];
  suppliers: any[] = [];

  constructor(
    private fb: FormBuilder,
    private categoryService: CategoryService,
    private supplierService: SupplierService
  ) { }

  ngOnInit(): void {

    this.form = this.fb.group({
      name: ['', [Validators.required, Validators.maxLength(120)]],
      description: [''],
      parentId: [null],
      suppliersIds: [[]]
    });

    this.loadDependencies();
  }

  ngOnDestroy(): void {
    this.destroy$.next();
    this.destroy$.complete();
  }

  /* =========================================================
     LOAD DEPENDENCIES (ENTERPRISE SAFE)
  ========================================================= */

  private loadDependencies(): void {

    forkJoin({
      categories: this.categoryService.getAll('tree', false),
      suppliers: this.supplierService.getAll(false)
    })
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: ({ categories, suppliers }) => {

          this.categories = this.flatten(categories);
          this.suppliers = suppliers;

          if (this.initialData) {
            this.patchInitialData();
          }

          this.loading = false;
        },
        error: () => {
          this.error = 'Failed to load dependencies.';
          this.loading = false;
        }
      });
  }

  /* =========================================================
     PATCH EDIT MODE
  ========================================================= */

  private patchInitialData(): void {
    if (!this.initialData) return;

    this.form.patchValue({
      name: this.initialData.name,
      description: this.initialData.description,
      parentId: this.initialData.parentId ?? null,
      suppliersIds: this.initialData.suppliers?.map(s => s.id) ?? []
    });
  }

  /* =========================================================
     TREE FLATTEN (SAFE)
  ========================================================= */

  private flatten(nodes: Category[]): Category[] {

    const result: Category[] = [];

    const traverse = (items: Category[]) => {
      for (const item of items) {

        if (this.initialData && item.id === this.initialData.id) {
          continue; // prevent self-parenting
        }

        result.push(item);

        if (item.subcategories?.length) {
          traverse(item.subcategories);
        }
      }
    };

    traverse(nodes);

    return result;
  }

  /* =========================================================
     SUBMIT
  ========================================================= */

  submit(): void {

    if (this.form.invalid || this.submitting) return;

    this.submitting = true;
    this.error = null;

    const payload = { ...this.form.value };

    this.submitted.emit({
      payload,
      done: () => this.submitting = false
    });
  }

  /* =========================================================
     HELPERS
  ========================================================= */

  get nameControl() {
    return this.form.get('name');
  }
}