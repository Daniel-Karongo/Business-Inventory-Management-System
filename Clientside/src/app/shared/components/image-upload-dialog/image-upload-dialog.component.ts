import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { ReactiveFormsModule, FormBuilder, Validators, FormGroup } from '@angular/forms';

@Component({
  selector: 'app-image-upload-dialog',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatDialogModule,
    MatButtonModule,
    MatIconModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule
  ],
  templateUrl: './image-upload-dialog.component.html',
  styleUrls: ['./image-upload-dialog.component.scss']
})
export class ImageUploadDialogComponent {

  file?: File;
  previewUrl?: string;

  descriptionOptions = ['ID', 'Passport', 'Signature', 'CV', 'Other'];

  fg!: FormGroup;

  constructor(
    private dialogRef: MatDialogRef<ImageUploadDialogComponent>,
    private fb: FormBuilder
  ) {
    // ✅ create the form AFTER fb is available
    this.fg = this.fb.group({
      type: ['ID', Validators.required],
      custom: ['']
    });

    this.fg.get('type')!.valueChanges.subscribe(value => {
      const custom = this.fg.get('custom');

      if (value === 'Other') {
        custom?.setValidators([Validators.required, Validators.minLength(3)]);
      } else {
        custom?.clearValidators();
        custom?.setValue('');
      }

      custom?.updateValueAndValidity();
    });
  }

  onFileSelected(event: Event) {
    const input = event.target as HTMLInputElement;
    const file = input.files?.[0];
    if (!file) return;

    this.file = file;

    this.previewUrl = file.type.startsWith('image/')
      ? URL.createObjectURL(file)
      : undefined;
  }

  confirm() {
    if (!this.file || this.fg.invalid) return;

    const type = this.fg.value.type;
    const description =
      type === 'Other'
        ? this.fg.value.custom
        : type;

    this.dialogRef.close({
      file: this.file,
      description
    });
  }

  cancel() {
    this.dialogRef.close();
  }
}