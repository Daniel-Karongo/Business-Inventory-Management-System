import { Component, OnInit } from '@angular/core';
import { CommonModule, Location } from '@angular/common';
import {
  FormBuilder,
  FormGroup,
  ReactiveFormsModule,
  Validators
} from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';

import { MatCardModule } from '@angular/material/card';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';
import { MatSnackBar } from '@angular/material/snack-bar';

import { SupplierService } from '../../services/supplier.service';
import { Supplier } from '../../models/supplier.model';

@Component({
  selector: 'app-supplier-edit',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatCardModule,
    MatFormFieldModule,
    MatInputModule,
    MatButtonModule
  ],
  templateUrl: './supplier-edit.component.html',
  styleUrls: ['./supplier-edit.component.scss']
})
export class SupplierEditComponent implements OnInit {

  form!: FormGroup;
  supplierId!: string;

  constructor(
    private fb: FormBuilder,
    private route: ActivatedRoute,
    private router: Router,
    private supplierService: SupplierService,
    private snackbar: MatSnackBar,
    private location: Location
  ) { }

  ngOnInit(): void {
    this.supplierId = this.route.snapshot.paramMap.get('identifier')!;
    this.buildForm();
    this.loadSupplier();
  }

  buildForm() {
    this.form = this.fb.group({
      name: ['', Validators.required],
      region: [''],
      rating: [null],
      emailsInput: [''],
      phonesInput: ['']
    });
  }

  loadSupplier() {
    const nav = this.router.getCurrentNavigation();
    let deleted = nav?.extras.state?.['deleted'] ?? false;

    this.supplierService.getById(this.supplierId, deleted)
      .subscribe(s => {
        this.patchForm(s)
      });
  }

  patchForm(s: Supplier) {
    this.form.patchValue({
      name: s.name,
      region: s.region,
      rating: s.rating,
      emailsInput: (s.email || []).join(', '),
      phonesInput: (s.phoneNumber || []).join(', ')
    });
  }

  submit() {
    const payload = {
      name: this.form.value.name,
      region: this.form.value.region,
      rating: this.form.value.rating,
      email: this.split(this.form.value.emailsInput),
      phoneNumber: this.split(this.form.value.phonesInput)
    };

    this.supplierService.update(this.supplierId, payload)
      .subscribe({
        next: () => {
          this.snackbar.open('Supplier updated', 'Close', { duration: 2000 });
          this.router.navigate(['/suppliers', this.supplierId]);
        }
      });
  }

  cancel() {
    this.location.back();
  }

  private split(v: string): string[] {
    return (v || '').split(',').map(x => x.trim()).filter(Boolean);
  }
}