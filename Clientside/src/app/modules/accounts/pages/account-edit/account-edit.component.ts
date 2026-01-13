import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ActivatedRoute, Router } from '@angular/router';
import { FormBuilder, Validators, ReactiveFormsModule, FormGroup } from '@angular/forms';

import { MatCardModule } from '@angular/material/card';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';

import { AccountsService } from '../../services/accounts.service';

@Component({
  selector: 'app-account-edit',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatCardModule,
    MatFormFieldModule,
    MatInputModule,
    MatButtonModule
  ],
  templateUrl: './account-edit.component.html',
  styleUrls: ['./account-edit.component.scss']
})
export class AccountEditComponent implements OnInit {

  private accountId!: string;

  form!: FormGroup;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private fb: FormBuilder,
    private accounts: AccountsService
  ) { }

  ngOnInit(): void {
    this.form = this.fb.group({
      code: [{ value: '', disabled: true }],
      name: ['', Validators.required],
      type: [{ value: '', disabled: true }]
    });


    const id = this.route.snapshot.paramMap.get('id');
    if (!id) return;

    this.accountId = id;

    this.accounts.get(id).subscribe(a => {
      this.form.patchValue(a);
    });
  }

  cancel() {
    this.router.navigate(['/accounts/chart']);
  }

  save() {
    if (this.form.invalid) return;

    this.accounts.rename(this.accountId, {
      name: this.form.value.name!
    }).subscribe(() => this.cancel());
  }
}