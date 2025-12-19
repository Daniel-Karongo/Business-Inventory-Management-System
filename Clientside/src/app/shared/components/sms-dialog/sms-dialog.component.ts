import { Component, Inject, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { FormControl, ReactiveFormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

import { SelectionModel } from '@angular/cdk/collections';

import { MatTableModule } from '@angular/material/table';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatOption, MatSelectModule } from '@angular/material/select';
import { MatPaginatorModule, PageEvent } from '@angular/material/paginator';

@Component({
  standalone: true,
  selector: 'app-sms-dialog',
  imports: [
    CommonModule,
    ReactiveFormsModule,

    MatTableModule,
    MatPaginatorModule,
    MatButtonModule,
    MatIconModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatCheckboxModule,
    MatTooltipModule,
  ],
  templateUrl: './sms-dialog.component.html',
  styleUrls: ['./sms-dialog.component.scss']
})
export class SmsDialogComponent implements OnInit {

  allCustomers: any[] = [];
  filteredCustomers: any[] = [];
  customers: any[] = [];

  selection = new SelectionModel<any>(true, []);

  searchCtrl = new FormControl('');
  messageCtrl = new FormControl('', { nonNullable: true });
  filterTypeCtrl = new FormControl<string>('');
  filterGenderCtrl = new FormControl<string>('');

  displayedColumns = ['select', 'name', 'phones', 'emails'];
  total = 0;
  page = 0;
  size = 10;

  constructor(
    @Inject(MAT_DIALOG_DATA) public data: { customers: any[] },
    private ref: MatDialogRef<SmsDialogComponent>
  ) { }

  ngOnInit(): void {
    this.allCustomers = this.data.customers ?? [];
    this.applyFilters();

    this.searchCtrl.valueChanges.subscribe(() => {
      this.page = 0;
      this.applyFilters();
    });

    this.filterTypeCtrl.valueChanges.subscribe(() => {
      if (this.filterTypeCtrl.value !== 'INDIVIDUAL') {
        this.filterGenderCtrl.reset('');
      }
      this.page = 0;
      this.applyFilters();
    });

    this.filterGenderCtrl.valueChanges.subscribe(() => {
      this.page = 0;
      this.applyFilters();
    });
  }

  applyFilters(): void {
    let data = [...this.allCustomers];

    const q = (this.searchCtrl.value || '').trim().toLowerCase();

    if (q) {
      data = data.filter(c =>
        (c.name || '').toLowerCase().includes(q) ||
        (c.phoneNumbers || []).some((p: string) => p.includes(q)) ||
        (c.email || []).some((e: string) => e.toLowerCase().includes(q))
      );
    }

    // TYPE FILTER
    if (this.filterTypeCtrl.value) {
      data = data.filter(c => c.type === this.filterTypeCtrl.value);
    }

    // GENDER FILTER
    if (this.filterGenderCtrl.value) {
      data = data.filter(c => c.gender === this.filterGenderCtrl.value);
    }

    // store filtered result
    this.filteredCustomers = data;
    this.total = data.length;

    // reset selection (important!)
    this.selection.clear();

    // apply pagination
    this.applyPagination();
  }

  applyPagination(): void {
    const start = this.page * this.size;
    const end = start + this.size;
    this.customers = this.filteredCustomers.slice(start, end);
  }

  toggleRow(row: any): void {
    this.selection.toggle(row);
  }

  isAllSelected(): boolean {
    return this.customers.length > 0 &&
      this.selection.selected.length === this.customers.length;
  }

  isIndeterminate(): boolean {
    return this.selection.selected.length > 0 &&
      this.selection.selected.length < this.customers.length;
  }

  toggleAll(): void {
    this.isAllSelected()
      ? this.selection.clear()
      : this.customers.forEach(c => this.selection.select(c));
  }

  onPageChange(e: PageEvent) {
    this.page = e.pageIndex;
    this.size = e.pageSize;
    this.applyPagination();
  }

  send(): void {
    if (!this.messageCtrl.value.trim() || !this.selection.selected.length) return;

    this.ref.close({
      message: this.messageCtrl.value.trim(),
      customerIds: this.selection.selected.map(c => c.id)
    });
  }

  close(): void {
    this.ref.close();
  }
}