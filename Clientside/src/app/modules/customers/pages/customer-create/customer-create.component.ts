import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import {
  FormBuilder,
  Validators,
  ReactiveFormsModule,
  FormGroup,
  FormArray
} from '@angular/forms';
import { Router, RouterModule } from '@angular/router';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';
import { MatChipsModule } from '@angular/material/chips';
import { MatSelectModule } from '@angular/material/select';
import { MatIconModule } from '@angular/material/icon';

import { CustomerService } from '../../services/customer.service';
import { MatSnackBar } from '@angular/material/snack-bar';

@Component({
  standalone: true,
  selector: 'app-customer-create',
  imports: [
    CommonModule,
    ReactiveFormsModule,

    MatFormFieldModule,
    MatInputModule,
    MatButtonModule,
    MatChipsModule,
    MatSelectModule,
    MatIconModule,
    RouterModule
  ],
  templateUrl: './customer-create.component.html',
  styleUrls: ['./customer-create.component.scss']
})
export class CustomerCreateComponent implements OnInit {

  fg!: FormGroup;

  constructor(
    private fb: FormBuilder,
    private service: CustomerService,
    private router: Router,
    private snackbar: MatSnackBar,
  ) { }

  ngOnInit(): void {
    this.fg = this.fb.group({
      name: ['', Validators.required],
      type: ['INDIVIDUAL', Validators.required],
      gender: [null],
      address: [''],
      notes: [''],

      phoneNumbers: [[]],
      emailAddresses: [[]]
    });

    this.fg.get('type')!.valueChanges.subscribe(t => {
      if (t === 'COMPANY') {
        this.fg.get('gender')!.reset();
      }
    });
  }

  /* ================= INPUT PARSING ================= */

  onPhonesChange(ev: Event) {
    const value = (ev.target as HTMLInputElement).value || '';
    const list = value
      .split(',')
      .map(v => v.trim())
      .filter(v => v.length >= 7);

    this.fg.get('phoneNumbers')!.setValue(list);
  }

  onEmailsChange(ev: Event) {
    const value = (ev.target as HTMLInputElement).value || '';
    const list = value
      .split(',')
      .map(v => v.trim())
      .filter(v => v.includes('@'));

    this.fg.get('emailAddresses')!.setValue(list);
  }

  /* ================= VALIDATION ================= */

  validateEmailsPhones(): boolean {
    const emails: string[] = this.fg.get('emailAddresses')!.value || [];
    const phones: string[] = this.fg.get('phoneNumbers')!.value || [];

    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    const phoneRegex = /^\+?\d{7,15}$/;

    for (const e of emails) {
      if (!emailRegex.test(e)) {
        this.snackbar.open(`Invalid email: ${e}`, 'Close', { duration: 1800 });
        return false;
      }
    }

    for (const p of phones) {
      if (!phoneRegex.test(p)) {
        this.snackbar.open(`Invalid phone number: ${p}`, 'Close', { duration: 1800 });
        return false;
      }
    }

    return true;
  }

  /* ===================== SAVE ===================== */

  save() {
    if (this.fg.invalid) return;
    if (!this.validateEmailsPhones()) return;

    this.service.create(this.fg.value).subscribe({
      next: () => {
        this.snackbar.open('Customer created successfully', 'Close', { duration: 2000 });
        this.router.navigate(['/customers']);
      },
      error: () =>
        this.snackbar.open('Failed to create customer', 'Close', { duration: 2000 })
    });
  }
}