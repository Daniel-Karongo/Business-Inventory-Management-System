import { Component, Input, Output, EventEmitter } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { CommonModule } from '@angular/common';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatChipsModule } from '@angular/material/chips';

import { SearchableAssignDialogComponent } from '../searchable-assign-dialog/searchable-assign-dialog.component';

@Component({
  selector: 'app-searchable-assign',
  standalone: true,
  imports: [
    CommonModule,
    MatButtonModule,
    MatIconModule,
    MatChipsModule
  ],
  templateUrl: './searchable-assign.component.html',
  styleUrls: ['./searchable-assign.component.scss']
})
export class SearchableAssignComponent<T> {

  @Input({ required: true }) label!: string;
  @Input({ required: true }) items: T[] = [];
  @Input() selectedIds: string[] = [];
  @Input() excludeIds: string[] = [];

  @Input({ required: true }) display!: (t: T) => string;
  @Input({ required: true }) id!: (t: T) => string;

  @Output() selectedIdsChange = new EventEmitter<string[]>();

  constructor(private dialog: MatDialog) {}

  openDialog() {
    const ref = this.dialog.open(SearchableAssignDialogComponent, {
      width: '420px',
      maxWidth: '95vw',
      panelClass: 'assign-dialog',
      data: {
        label: this.label,
        items: this.items,
        selectedIds: this.selectedIds,
        excludeIds: this.excludeIds,
        display: this.display,
        id: this.id
      }
    });

    ref.afterClosed().subscribe(result => {
      if (Array.isArray(result)) {
        this.selectedIdsChange.emit(result);
      }
    });
  }

  remove(id: string) {
    this.selectedIdsChange.emit(this.selectedIds.filter(x => x !== id));
  }

  selectedItems(): T[] {
    return this.items.filter(i => this.selectedIds.includes(this.id(i)));
  }
}