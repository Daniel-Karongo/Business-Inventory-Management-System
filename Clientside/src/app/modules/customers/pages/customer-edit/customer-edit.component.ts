import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ActivatedRoute, Router, RouterModule } from '@angular/router';
import {
  FormBuilder,
  FormGroup,
  Validators,
  ReactiveFormsModule,
  FormArray
} from '@angular/forms';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';
import { MatChipsModule } from '@angular/material/chips';
import { MatIconModule } from '@angular/material/icon';

import { CustomerService } from '../../services/customer.service';

@Component({
  standalone: true,
  selector: 'app-customer-edit',
  imports: [
    CommonModule,
    ReactiveFormsModule,
    RouterModule,

    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatButtonModule,
    MatChipsModule,
    MatIconModule
  ],
  templateUrl: './customer-edit.component.html',
  styleUrls: ['./customer-edit.component.scss']
})
export class CustomerEditComponent implements OnInit {

  id!: string;
  fg!: FormGroup;

  constructor(
    private route: ActivatedRoute,
    private fb: FormBuilder,
    private service: CustomerService,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.fg = this.fb.group({
      name: ['', Validators.required],
      type: ['INDIVIDUAL', Validators.required],
      gender: [null],
      address: [''],
      notes: [''],

      phoneNumbers: this.fb.array([]),
      emailAddresses: this.fb.array([])
    });

    this.fg.get('type')!.valueChanges.subscribe(t => {
      if (t === 'COMPANY') {
        this.fg.get('gender')!.reset();
      }
    });

    this.id = this.route.snapshot.paramMap.get('id')!;
    this.service.get(this.id).subscribe(c => this.patch(c));
  }

  get phoneNumbers(): FormArray {
    return this.fg.get('phoneNumbers') as FormArray;
  }

  get emailAddresses(): FormArray {
    return this.fg.get('emailAddresses') as FormArray;
  }

  patch(c: any) {
    this.fg.patchValue(c);

    this.phoneNumbers.clear();
    this.emailAddresses.clear();

    c.phoneNumbers?.forEach((p: string) =>
      this.phoneNumbers.push(this.fb.control(p))
    );

    c.emailAddresses?.forEach((e: string) =>
      this.emailAddresses.push(this.fb.control(e))
    );
  }

  addPhone(ev: any) {
    const value = ev.value?.trim();
    if (!value) return;
    this.phoneNumbers.push(this.fb.control(value));
    ev.chipInput?.clear();
  }

  removePhone(i: number) {
    this.phoneNumbers.removeAt(i);
  }

  addEmail(ev: any) {
    const value = ev.value?.trim();
    if (!value) return;
    this.emailAddresses.push(this.fb.control(value));
    ev.chipInput?.clear();
  }

  removeEmail(i: number) {
    this.emailAddresses.removeAt(i);
  }

  save() {
    if (this.fg.invalid) return;

    this.service.update(this.id, this.fg.value)
      .subscribe(() => this.router.navigate(['/customers', this.id]));
  }
}