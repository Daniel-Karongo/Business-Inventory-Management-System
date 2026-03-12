import { Component, Input } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatButtonModule } from '@angular/material/button';

import { Product } from '../../models/product.model';
import { VariantListComponent } from '../../../variant/components/variant-list/variant-list.component';

@Component({
  selector: 'app-variant-summary',
  standalone: true,
  imports: [
    CommonModule,
    MatButtonModule,
    VariantListComponent
  ],
  templateUrl: './variant-summary.component.html',
  styleUrls: ['./variant-summary.component.scss']
})
export class VariantSummaryComponent {

  @Input({ required: true }) product!: Product;

}