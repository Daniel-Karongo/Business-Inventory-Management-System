import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import {
  FormArray,
  FormBuilder,
  FormGroup,
  ReactiveFormsModule,
  Validators
} from '@angular/forms';
import { Router } from '@angular/router';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import { AccountsService } from '../../services/accounts.service';
import { ManualJournalService } from '../../services/manual-journal.service';

@Component({
  selector: 'app-manual-journal',
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
  templateUrl: './manual-journal.component.html',
  styleUrls: ['./manual-journal.component.scss']
})
export class ManualJournalComponent implements OnInit {

  form!: FormGroup;
  accounts$!: ReturnType<AccountsService['list']>;
  posting = false;

  constructor(
    private fb: FormBuilder,
    private accountsService: AccountsService,
    private journalService: ManualJournalService,
    private snackbar: MatSnackBar,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.accounts$ = this.accountsService.list();

    this.form = this.fb.group({
      reference: ['', Validators.required],
      description: [''],
      lines: this.fb.array([])
    });

    // Minimum double-entry structure
    this.addLine();
    this.addLine();
  }

  get lines(): FormArray {
    return this.form.get('lines') as FormArray;
  }

  addLine(): void {
    this.lines.push(
      this.fb.group({
        accountId: ['', Validators.required],
        direction: ['DEBIT', Validators.required],
        amount: [null, [Validators.required, Validators.min(0.01)]]
      })
    );
  }

  removeLine(index: number): void {
    this.lines.removeAt(index);
  }

  submit(): void {
    if (this.form.invalid || this.posting) return;

    this.posting = true;

    this.journalService.post(this.form.value).subscribe({
      next: () => {
        this.snackbar.open('Journal posted successfully', 'Close', {
          duration: 2500
        });
        this.router.navigate(['/accounts/journals']);
      },
      error: () => {
        this.snackbar.open('Failed to post journal', 'Close', {
          duration: 3000
        });
        this.posting = false;
      }
    });
  }

  cancel(): void {
    this.router.navigate(['/accounts/journals']);
  }
}