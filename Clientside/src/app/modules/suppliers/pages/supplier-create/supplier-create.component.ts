import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import {
  FormArray,
  FormBuilder,
  FormGroup,
  Validators,
  ReactiveFormsModule
} from '@angular/forms';
import { Router } from '@angular/router';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import { finalize } from 'rxjs/operators';

import { SupplierService } from '../../services/supplier.service';
import { CategoryService } from '../../../categories/services/category.service';
import { Category } from '../../../categories/models/category.model';
import { FileViewerDialog } from '../../../../shared/components/file-viewer/file-viewer.component';
import { MatDialog } from '@angular/material/dialog';

@Component({
  selector: 'app-supplier-create',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatButtonModule,
    MatIconModule,
    MatSnackBarModule
  ],
  templateUrl: './supplier-create.component.html',
  styleUrls: ['./supplier-create.component.scss']
})
export class SupplierCreateComponent implements OnInit {

  steps = ['Basic', 'Contacts', 'Categories', 'Files', 'Review'];
  step = 0;

  form!: FormGroup;
  categories: Category[] = [];
  busy = false;

  files: {
    file: File;
    description: string;
    custom?: string;
  }[] = [];

  descriptionOptions = [
    'Contract',
    'Price List',
    'Certificate',
    'License',
    'Other'
  ];

  constructor(
    private fb: FormBuilder,
    private supplierService: SupplierService,
    private categoryService: CategoryService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog,
    public router: Router
  ) { }

  ngOnInit() {
    this.form = this.fb.group({
      name: ['', Validators.required],
      region: [''],
      rating: [null],

      email: [[]],
      phoneNumber: [[]],

      categoryIds: [[]],

      supplierFiles: this.fb.array([])
    });

    this.loadCategories();
  }

  get supplierFiles(): FormArray {
    return this.form.get('supplierFiles') as FormArray;
  }

  newSupplierFile(file: File): FormGroup {
    const previewUrl =
      file.type.startsWith('image/')
        ? URL.createObjectURL(file)
        : null;

    const fg = this.fb.group({
      file: [file],
      previewUrl: [previewUrl],
      description: ['Contract', Validators.required],
      custom: ['']
    });

    fg.get('description')!.valueChanges.subscribe(v => {
      const custom = fg.get('custom');
      if (v === 'Other') {
        custom?.setValidators([Validators.required, Validators.minLength(3)]);
      } else {
        custom?.clearValidators();
        custom?.setValue('');
      }
      custom?.updateValueAndValidity();
    });

    return fg;
  }

  openPdf(file: File) {
    const url = URL.createObjectURL(file);
    window.open(url, '_blank');
  }

  /* ============================================================
     LOAD
  ============================================================ */

  loadCategories() {
    this.categoryService.getAll('tree', false)
      .subscribe(c => this.categories = c || []);
  }

  /* ============================================================
     CONTACT PARSING
  ============================================================ */

  onEmailsChange(event: Event) {
    const input = event.target as HTMLInputElement | null;
    if (!input) return;

    const value = input.value || '';
    this.form.get('email')!.setValue(
      value.split(',')
        .map(v => v.trim())
        .filter(Boolean)
    );
  }

  onPhonesChange(event: Event) {
    const input = event.target as HTMLInputElement | null;
    if (!input) return;

    const value = input.value || '';
    this.form.get('phoneNumber')!.setValue(
      value.split(',')
        .map(v => v.trim())
        .filter(Boolean)
    );
  }

  /* ============================================================
     CATEGORY TREE SELECTION
  ============================================================ */

  toggleCategory(id: number) {
    const list = this.form.value.categoryIds as number[];
    this.form.patchValue({
      categoryIds: list.includes(id)
        ? list.filter(x => x !== id)
        : [...list, id]
    });
  }

  isCategorySelected(id: number): boolean {
    return this.form.value.categoryIds.includes(id);
  }

  /* ============================================================
     FILES
  ============================================================ */

  onFilesPicked(ev: Event) {
    const input = ev.target as HTMLInputElement;
    if (!input.files) return;

    Array.from(input.files).forEach(file => {
      this.supplierFiles.push(this.newSupplierFile(file));
    });

    input.value = '';
  }

  removeFile(i: number) {
    const fg = this.supplierFiles.at(i);
    const url = fg.value.previewUrl;
    if (url) URL.revokeObjectURL(url);

    this.supplierFiles.removeAt(i);
  }

  private validateFiles(): boolean {
    for (const f of this.files) {
      if (!f.description) return false;
      if (f.description === 'Other' && !f.custom?.trim()) return false;
    }
    return true;
  }

  viewImage(file: File, previewUrl: string) {
    this.dialog.open(FileViewerDialog, {
      data: {
        preview: {
          src: previewUrl,
          name: file.name,
          type: 'image'
        }
      },
      width: '80%',
      maxWidth: '1100px'
    });
  }

  /* ============================================================
     NAV
  ============================================================ */

  next() {
    if (this.step < this.steps.length - 1) this.step++;
  }

  prev() {
    if (this.step > 0) this.step--;
  }

  /* ============================================================
     SUBMIT
  ============================================================ */

  submit() {
    const fd = new FormData();

    Object.entries(this.form.value).forEach(([k, v]) => {

      // 🔒 skip file FormArray — handled separately
      if (k === 'supplierFiles') return;

      if (v === null || v === undefined) return;

      if (Array.isArray(v)) {
        v.forEach((x, i) => {
          fd.append(`${k}[${i}]`, String(x));
        });
        return;
      }

      fd.append(k, String(v));
    });

    if (!this.validateFiles()) {
      this.snackbar.open(
        'Please provide a description for all uploaded files',
        'Close',
        { duration: 3000 }
      );
      return;
    }

    this.supplierFiles.controls.forEach((fg, i) => {
      const file: File = fg.value.file;
      const description =
        fg.value.description === 'Other'
          ? fg.value.custom
          : fg.value.description;

      fd.append(`supplierFiles[${i}].file`, file, file.name);
      fd.append(`supplierFiles[${i}].description`, description);
    });

    this.busy = true;

    this.supplierService.create(fd)
      .pipe(finalize(() => this.busy = false))
      .subscribe({
        next: () => {
          this.snackbar.open('Supplier created', 'Close', { duration: 2000 });
          this.router.navigate(['/suppliers']);
        },
        error: () =>
          this.snackbar.open('Failed to create supplier', 'Close', { duration: 3000 })
      });
  }
}