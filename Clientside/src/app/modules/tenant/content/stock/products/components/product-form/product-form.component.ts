interface ProductAssignedFile {

  id: string;

  file?: File;

  existing?: boolean;

  imageId?: string;

  fileName: string;

  previewUrl?: string;

  assignToProduct: boolean;

  variantClassifications: string[];

  description?: string;
}

import {
  Component,
  EventEmitter,
  Input,
  OnDestroy,
  OnInit,
  Output,
  inject
} from '@angular/core';

import { CommonModule } from '@angular/common';

import {
  FormArray,
  FormBuilder,
  FormGroup,
  ReactiveFormsModule,
  Validators
} from '@angular/forms';

import { MatTabsModule } from '@angular/material/tabs';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';

import { BranchService }
  from '../../../../branches/services/branch.service';

import { BranchContextService }
  from '../../../../../../../core/services/branch-context.service';

import { SupplierService }
  from '../../../../suppliers/services/supplier.service';

import { CategoryService }
  from '../../../categories/services/category.service';

import { Supplier }
  from '../../../../suppliers/models/supplier.model';

import { Category }
  from '../../../categories/models/category.model';
import { SearchableAssignComponent } from '../../../../../../../shared/components/searchable-assign/searchable-assign.component';
import { BulkCameraCaptureService } from '../../../../../../../shared/bulk-import/camera/bulk-camera-capture.service';
import { MatCheckboxModule } from '@angular/material/checkbox';

@Component({
  selector: 'app-product-form',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatTabsModule,
    MatButtonModule,
    MatInputModule,
    MatFormFieldModule,
    MatIconModule,
    MatCheckboxModule,
    SearchableAssignComponent
  ],
  templateUrl: './product-form.component.html',
  styleUrls: ['./product-form.component.scss']
})
export class ProductFormComponent implements OnInit, OnDestroy {

  @Output()
  submitForm =
    new EventEmitter<FormData>();

  @Output()
  cancel =
    new EventEmitter<void>();

  @Input()
  initialValue: any = null;

  @Input()
  editMode = false;

  form: FormGroup;

  branches: any[] = [];

  categories: Category[] = [];

  suppliers: Supplier[] = [];

  showBranchField = true;

  files: ProductAssignedFile[] = [];

  private branchService =
    inject(BranchService);

  private branchContext =
    inject(BranchContextService);

  private categoryService =
    inject(CategoryService);

  private supplierService =
    inject(SupplierService);

  private camera =
    inject(BulkCameraCaptureService);

  constructor(
    private fb: FormBuilder
  ) {

    this.form = this.fb.group({

      name: [
        '',
        Validators.required
      ],

      description: [''],

      categoryId: [
        null,
        Validators.required
      ],

      supplierIds: [[]],

      sku: [''],

      branchId: [
        null,
        Validators.required
      ],

      minimumPercentageProfit: [null],

      minimumProfit: [null],

      variants:
        this.fb.array([])

    });

    this.addVariant('STANDARD');
  }

  ngOnInit(): void {

    this.loadBranches();

    this.loadCategories();

    this.loadSuppliers();

    if (this.initialValue) {
      this.patchExistingProduct();
    }
  }

  ngOnDestroy(): void {

    this.files.forEach(file => {

      if (
        file.previewUrl?.startsWith(
          'blob:'
        )
      ) {

        URL.revokeObjectURL(
          file.previewUrl
        );
      }
    });
  }

  trackVariant(
    index: number,
    group: any
  ) {
    return group.value.tempKey;
  }

  private loadBranches() {

    this.branchService
      .getAllLegacy()
      .subscribe(branches => {

        this.branches =
          branches ?? [];

        this.showBranchField =
          this.branches.length > 1;

        const defaultBranch =
          this.initialValue?.branchId ??
          this.branchContext.currentBranch;

        this.form.patchValue({
          branchId:
            defaultBranch
        });

        if (this.editMode) {

          this.form
            .get('branchId')
            ?.disable();
        }
      });
  }

  private loadCategories() {

    this.categoryService
      .getAll('flat', false)
      .subscribe(categories => {

        this.categories =
          categories ?? [];
      });
  }

  private loadSuppliers() {

    this.supplierService
      .getAll(false)
      .subscribe(suppliers => {

        this.suppliers =
          suppliers ?? [];
      });
  }

  async capturePhotos() {

    const files =
      await this.camera.capture(
        'product',
        0
      );

    if (!files?.length) {
      return;
    }

    files.forEach(file => {

      this.files.push({

        id:
          crypto.randomUUID(),

        file,

        existing: false,

        fileName:
          file.name,

        previewUrl:
          URL.createObjectURL(file),

        assignToProduct:
          true,

        variantClassifications: [
          'STANDARD'
        ]
      });

    });
  }

  private patchExistingProduct() {

    this.form.patchValue({

      name:
        this.initialValue.name,

      description:
        this.initialValue.description,

      sku:
        this.initialValue.sku,

      categoryId:
        this.initialValue.categoryId,

      supplierIds:
        (this.initialValue.suppliers ?? [])
          .map((s: any) => s.id),

      minimumPercentageProfit:
        this.initialValue.minimumPercentageProfit,

      minimumProfit:
        this.initialValue.minimumProfit

    });

    this.variants.clear();

    (this.initialValue.variants ?? [])
      .forEach((variant: any) => {

        this.variants.push(

          this.fb.group({
            id:
              variant.id,

            tempKey:
              crypto.randomUUID(),

            classification: [
              variant.classification,
              Validators.required
            ],

            barcode: [
              variant.barcode ?? ''
            ],

            sku: [
              variant.sku ?? ''
            ],

            minimumPercentageProfit: [
              variant.minimumPercentageProfit ?? null
            ],

            minimumProfit: [
              variant.minimumProfit ?? null
            ]

          })
        );
      });

    this.files =
      (this.initialValue.images ?? [])
        .map((img: any) => ({

          id:
            img.id,

          existing:
            true,

          imageId:
            img.id,

          fileName:
            img.fileName,

          previewUrl:
            img.filePath,

          assignToProduct:
            true,

          variantClassifications: []
        }));
  }

  get allowAssignments(): boolean {
    return !this.editMode;
  }

  get variants(): FormArray<FormGroup> {
    return this.form.get(
      'variants'
    ) as FormArray<FormGroup>;
  }

  get variantGroups(): FormGroup[] {

    return this.variants
      .controls as FormGroup[];
  }

  addVariant(
    classification = ''
  ) {

    this.variants.push(

      this.fb.group({
        id: null,

        tempKey:
          crypto.randomUUID(),

        classification: [
          classification,
          Validators.required
        ],

        barcode: [''],

        sku: [''],

        minimumPercentageProfit: [null],

        minimumProfit: [null]

      })
    );
  }

  removeVariant(
    index: number
  ) {

    if (
      this.variants.length <= 1
    ) {
      return;
    }

    const classification =
      this.variants.at(index)
        .get('classification')
        ?.value;

    this.files.forEach(file => {

      file.variantClassifications =
        file.variantClassifications
          .filter(v =>
            v !== classification
          );
    });

    this.variants.removeAt(index);
  }

  toggleVariantAssignment(
    file: ProductAssignedFile,
    classification: string,
    checked: boolean
  ) {

    if (checked) {

      if (
        !file.variantClassifications
          .includes(classification)
      ) {

        file.variantClassifications
          .push(classification);
      }

      return;
    }

    file.variantClassifications =
      file.variantClassifications
        .filter(v =>
          v !== classification
        );
  }

  isVariantAssigned(
    file: ProductAssignedFile,
    classification: string
  ): boolean {

    return file
      .variantClassifications
      .includes(classification);
  }

  addFiles(event: Event) {

    const input =
      event.target as HTMLInputElement;

    const files =
      Array.from(
        input.files ?? []
      );

    files.forEach(file => {

      this.files.push({

        id:
          crypto.randomUUID(),

        file,

        existing: false,

        fileName:
          file.name,

        previewUrl:
          file.type.startsWith('image/')
            ? URL.createObjectURL(file)
            : undefined,

        assignToProduct:
          true,

        variantClassifications: [
          'STANDARD'
        ]
      });
    });

    input.value = '';
  }

  removeFile(index: number) {

    const file =
      this.files[index];

    if (
      file?.previewUrl?.startsWith(
        'blob:'
      )
    ) {
      URL.revokeObjectURL(
        file.previewUrl
      );
    }

    this.files.splice(
      index,
      1
    );
  }

  categoryDisplay =
    (c: Category) => c.name;

  categoryIdFn =
    (c: Category) =>
      String(c.id);

  onCategorySelected(
    ids: string[]
  ) {

    this.form.patchValue({

      categoryId:
        ids.length
          ? Number(ids[0])
          : null

    });
  }

  supplierDisplay =
    (s: Supplier) =>
      s.name;

  supplierIdFn =
    (s: Supplier) =>
      s.id;

  onSuppliersSelected(
    ids: string[]
  ) {

    this.form.patchValue({
      supplierIds: ids
    });
  }

  selectedCategoryName(): string {

    const id =
      this.form.value.categoryId;

    return this.categories.find(
      c => c.id === id
    )?.name ?? '';
  }

  selectedSupplierNames(): string[] {

    const ids =
      this.form.value.supplierIds ?? [];

    return this.suppliers
      .filter(s => ids.includes(s.id))
      .map(s => s.name);
  }

  save() {

    if (this.form.invalid) {

      this.form.markAllAsTouched();

      return;
    }

    const payload = {
      product: {
        name: this.form.value.name,
        description: this.form.value.description,
        categoryId: this.form.value.categoryId,
        supplierIds: this.form.value.supplierIds,
        sku:
          this.form.value.sku?.trim()
            ? this.form.value.sku.trim()
            : null,
        minimumPercentageProfit: this.form.value.minimumPercentageProfit,
        minimumProfit: this.form.value.minimumProfit
      },

      variants:
        this.form.getRawValue()
          .variants
          .map((v: any) => {

            if (this.editMode) {
              return {
                id: v.id,
                classification: v.classification,
                sku:
                  v.sku?.trim()
                    ? v.sku.trim()
                    : null,
                minimumPercentageProfit:
                  v.minimumPercentageProfit,
                minimumProfit:
                  v.minimumProfit,
                autoCreateBasePackaging: true
              };
            }

            return {
              classification: v.classification,
              barcode: v.barcode,
              sku: v.sku,
              minimumPercentageProfit:
                v.minimumPercentageProfit,
              minimumProfit:
                v.minimumProfit,
              autoCreateBasePackaging: true
            };
          }),

      fileAssignments:

        this.files.map(f => ({

          fileName:
            f.fileName,

          assignToProduct:
            f.assignToProduct,

          variantClassifications:
            f.variantClassifications

        }))
    };

    const formData =
      new FormData();

    formData.append(
      'payload',
      new Blob(
        [
          JSON.stringify(payload)
        ],
        {
          type:
            'application/json'
        }
      )
    );

    this.files.forEach(file => {

      if (!file.file) {
        return;
      }

      formData.append(
        'files',
        file.file
      );
    });

    this.submitForm.emit(
      formData
    );
  }
}