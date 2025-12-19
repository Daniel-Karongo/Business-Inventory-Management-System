import { Component, Inject, ViewChild } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef, MatDialogModule } from '@angular/material/dialog';
import { FormControl, ReactiveFormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatFormFieldModule } from '@angular/material/form-field';
import { CdkVirtualScrollViewport, ScrollingModule } from '@angular/cdk/scrolling';

@Component({
  standalone: true,
  imports: [
    CommonModule,
    MatDialogModule,
    MatButtonModule,
    MatIconModule,
    MatInputModule,
    MatFormFieldModule,
    ReactiveFormsModule,
    ScrollingModule
  ],
  templateUrl: './searchable-assign-dialog.component.html',
  styleUrls: ['./searchable-assign-dialog.component.scss']
})
export class SearchableAssignDialogComponent<T> {

  search = new FormControl('', { nonNullable: true });
  selected = new Set<string>();
  activeIndex = 0;

  @ViewChild(CdkVirtualScrollViewport) viewport!: CdkVirtualScrollViewport;

  constructor(
    @Inject(MAT_DIALOG_DATA) public data: {
      label: string;
      items: T[];
      selectedIds: string[];
      excludeIds: string[];
      display: (t: T) => string;
      id: (t: T) => string;
    },
    private ref: MatDialogRef<SearchableAssignDialogComponent<T>>
  ) {
    data.selectedIds.forEach(id => this.selected.add(id));
  }

  get filtered(): T[] {
    const q = this.search.value.toLowerCase();
    return this.data.items
      .filter(i => !this.data.excludeIds.includes(this.data.id(i)))
      .filter(i => this.data.display(i).toLowerCase().includes(q));
  }

  toggle(item: T) {
    const id = this.data.id(item);
    this.selected.has(id) ? this.selected.delete(id) : this.selected.add(id);
  }

  selectAll() {
    this.filtered.forEach(i => this.selected.add(this.data.id(i)));
  }

  clearAll() {
    this.filtered.forEach(i => this.selected.delete(this.data.id(i)));
  }

  handleKey(event: KeyboardEvent) {
    if (event.key === 'ArrowDown') this.activeIndex++;
    if (event.key === 'ArrowUp') this.activeIndex--;
    if (event.key === 'Enter') this.toggle(this.filtered[this.activeIndex]);
    if (event.key === 'Escape') this.ref.close();

    this.activeIndex = Math.max(0, Math.min(this.activeIndex, this.filtered.length - 1));
    this.viewport.scrollToIndex(this.activeIndex);
  }

  apply() {
    this.ref.close([...this.selected]);
  }

  cancel() {
    this.ref.close();
  }
}