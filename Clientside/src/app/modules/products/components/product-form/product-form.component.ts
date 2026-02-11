import { Component, EventEmitter, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import {
  FormBuilder,
  FormGroup,
  FormArray,
  ReactiveFormsModule,
  Validators
} from '@angular/forms';

import { MatTabsModule } from '@angular/material/tabs';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';

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
    MatIconModule
  ],
  templateUrl: './product-form.component.html',
  styleUrls: ['./product-form.component.scss']
})
export class ProductFormComponent {

  @Output() submitForm = new EventEmitter<FormData>();
  @Output() cancel = new EventEmitter<void>();

  form: FormGroup;

  files: any[] = [];

  constructor(private fb: FormBuilder) {

    this.form = this.fb.group({
      name: ['', Validators.required],
      description: [''],
      categoryId: [null, Validators.required],
      minimumPercentageProfit: [],
      variants: this.fb.array([])
    });

    this.addVariant('STANDARD');
  }

  get variants(): FormArray {
    return this.form.get('variants') as FormArray;
  }

  get variantGroups(): FormGroup[] {
    return this.variants.controls as FormGroup[];
  }

  addVariant(name = '') {
    this.variants.push(
      this.fb.group({
        tempKey: crypto.randomUUID(),
        classification: [name, Validators.required],
        barcode: ['']
      })
    );
  }

  removeVariant(index: number) {
    this.variants.removeAt(index);
  }

  addFiles(event: any) {
    const fileList = Array.from(event.target.files) as File[];

    fileList.forEach(file => {
      this.files.push({
        id: crypto.randomUUID(),
        file,
        target: 'PRODUCT',
        variantKeys: []
      });
    });
  }

  removeFile(index: number) {
    this.files.splice(index, 1);
  }

  save() {

    const payload = {
      product: {
        name: this.form.value.name,
        description: this.form.value.description,
        categoryId: this.form.value.categoryId,
        minimumPercentageProfit: this.form.value.minimumPercentageProfit
      },
      variants: this.form.value.variants,
      fileAssignments: this.files.map(f => ({
        fileName: f.file.name,
        target: f.target,
        variantClassifications: this.form.value.variants
          .filter((v: any) => f.variantKeys.includes(v.tempKey))
          .map((v: any) => v.classification)
      }))
    };

    const formData = new FormData();

    formData.append(
      'payload',
      new Blob([JSON.stringify(payload)], { type: 'application/json' })
    );

    this.files.forEach(f => {
      formData.append('files', f.file);
    });

    this.submitForm.emit(formData);
  }
}