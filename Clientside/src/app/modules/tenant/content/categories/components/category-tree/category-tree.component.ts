import { CommonModule } from '@angular/common';
import { ChangeDetectionStrategy, Component, EventEmitter, Input, Output } from '@angular/core';
import { MatIconModule } from '@angular/material/icon';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { Router, RouterModule } from '@angular/router';
import { Category } from '../../models/category.model';
import { MatTooltipModule } from '@angular/material/tooltip';

@Component({
  selector: 'app-category-tree',
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    MatIconModule,
    MatCheckboxModule,
    RouterModule,
    MatTooltipModule
  ],
  templateUrl: './category-tree.component.html',
  styleUrls: ['./category-tree.component.scss']
})
export class CategoryTreeComponent {

  @Input() activeId?: number;
  @Input() categories: Category[] = [];
  @Input() expandedIds!: Set<number>;
  @Input() selectedIds!: Set<number>;

  @Output() select = new EventEmitter<number>();

  constructor(private router: Router) { }

  trackById(_: number, item: Category) {
    return item.id;
  }

  navigate(id: number) {
    this.router.navigate(['/app/categories', id]);
  }

  toggleExpand(id: number) {
    const next = new Set(this.expandedIds);
    next.has(id) ? next.delete(id) : next.add(id);
    this.expandedIds = next;
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
