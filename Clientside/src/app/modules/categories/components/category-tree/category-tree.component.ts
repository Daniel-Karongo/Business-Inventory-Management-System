import { CommonModule } from '@angular/common';
import { Component, EventEmitter, Input, Output } from '@angular/core';
import { MatIconModule } from '@angular/material/icon';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { Router, RouterModule } from '@angular/router';
import { Category } from '../../models/category.model';

@Component({
  selector: 'app-category-tree',
  standalone: true,
  imports: [
    CommonModule,
    MatIconModule,
    MatCheckboxModule,
    RouterModule
  ],
  templateUrl: './category-tree.component.html',
  styleUrls: ['./category-tree.component.scss']
})
export class CategoryTreeComponent {

  @Input() categories: Category[] = [];
  @Input() expandedIds!: Set<number>;
  @Input() selectedIds!: Set<number>;

  @Output() select = new EventEmitter<number>();

  constructor(private router: Router) { }

  navigate(id: number) {
    this.router.navigate(['/categories', id]);
  }

  toggleExpand(id: number) {
    if (this.expandedIds.has(id)) {
      this.expandedIds.delete(id);
    } else {
      this.expandedIds.add(id);
    }

    this.expandedIds = new Set(this.expandedIds);
  }

  toggleSelect(id: number) {
    this.select.emit(id);
  }

  isIndeterminate(cat: Category): boolean {
    if (!cat.subcategories?.length) return false;

    const selectedChildren = cat.subcategories.filter(s =>
      this.selectedIds.has(s.id)
    ).length;

    return selectedChildren > 0 &&
      selectedChildren < cat.subcategories.length;
  }
}