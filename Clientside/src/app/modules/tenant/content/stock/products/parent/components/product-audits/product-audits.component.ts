import {
  Component,
  Input,
  OnChanges,
  OnInit,
  SimpleChanges
} from '@angular/core';

import {
  CommonModule
} from '@angular/common';

import {
  FormsModule
} from '@angular/forms';

import {
  MatIconModule
} from '@angular/material/icon';

import {
  MatProgressSpinnerModule
} from '@angular/material/progress-spinner';

import {
  WorkflowCardComponent
} from '../../../../../../../../shared/layout/workflow-card/workflow-card.component';

import {
  ProductService
} from '../../services/product.service';

import {
  ProductAudit
} from '../../../../models/product.model';

@Component({
  selector: 'app-product-audits',
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    MatIconModule,
    MatProgressSpinnerModule,
    WorkflowCardComponent
  ],
  templateUrl:
    './product-audits.component.html',
  styleUrls: [
    './product-audits.component.scss'
  ]
})
export class ProductAuditsComponent
  implements OnInit, OnChanges {

  @Input({
    required: true
  })
  productId!: string;

  audits: ProductAudit[] = [];

  loading = true;

  selectedAction = 'ALL';

  selectedField = 'ALL';

  constructor(
    private productService:
      ProductService
  ) { }

  ngOnInit(): void {

    this.loadAudits();
  }

  loadAudits(): void {

    this.loading = true;

    this.productService
      .getAudits(
        this.productId
      )
      .subscribe({
        next: audits => {

          this.audits =
            audits ?? [];

          this.loading =
            false;
        },
        error: () => {

          this.loading =
            false;
        }
      });
  }

  ngOnChanges(
    changes: SimpleChanges
  ): void {
    if (
      changes['productId'] &&
      !changes['productId'].firstChange
    ) {
      this.loadAudits();
    }
  }

  public reload(): void {
    this.loadAudits();
  }

  trackAudit(
    _: number,
    audit: ProductAudit
  ): string {

    return (
      audit.timestamp +
      audit.action +
      (audit.fieldChanged ?? '')
    );
  }

  get uniqueActions(): string[] {

    return Array.from(
      new Set(
        this.audits.map(
          a => a.action
        )
      )
    );
  }

  get uniqueFields(): string[] {

    return Array.from(
      new Set(
        this.audits
          .map(
            a => a.fieldChanged
          )
          .filter(
            (
              field
            ): field is string =>
              !!field
          )
      )
    );
  }

  get filteredAudits():
    ProductAudit[] {

    return this.audits.filter(
      audit => {

        const actionMatch =
          this.selectedAction ===
          'ALL'
          ||
          audit.action ===
          this.selectedAction;

        const fieldMatch =
          this.selectedField ===
          'ALL'
          ||
          audit.fieldChanged ===
          this.selectedField;

        return (
          actionMatch
          &&
          fieldMatch
        );
      }
    );
  }

  displayAction(
    action: string
  ): string {

    return ACTION_LABELS[
      action
    ]
      ??
      action
        .toLowerCase()
        .replace(
          /_/g,
          ' '
        )
        .replace(
          /\b\w/g,
          c =>
            c.toUpperCase()
        );
  }

  getActionClass(
    action: string
  ): string {

    switch (action) {

      case 'CREATE':
        return 'create';

      case 'UPDATE':
        return 'update';

      case 'SOFT_DELETE':
      case 'HARD_DELETE':
        return 'delete';

      case 'RESTORE':
        return 'restore';

      default:
        return 'default';
    }
  }

  getActionIcon(
    action: string
  ): string {

    switch (action) {

      case 'CREATE':
        return 'add_box';

      case 'UPDATE':
        return 'edit';

      case 'SOFT_DELETE':
        return 'delete';

      case 'HARD_DELETE':
        return 'delete_forever';

      case 'RESTORE':
        return 'restore';

      default:
        return 'history';
    }
  }

  hasDiff(
    audit: ProductAudit
  ): boolean {

    return !!(
      audit.oldValue
      ||
      audit.newValue
    );
  }
}

const ACTION_LABELS:
  Record<string, string> = {

  CREATE:
    'Product Created',

  UPDATE:
    'Product Updated',

  SOFT_DELETE:
    'Soft Deleted',

  HARD_DELETE:
    'Permanently Deleted',

  RESTORE:
    'Restored'
};