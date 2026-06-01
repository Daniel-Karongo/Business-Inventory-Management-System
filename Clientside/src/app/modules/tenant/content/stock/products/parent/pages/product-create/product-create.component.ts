import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { Router } from '@angular/router';
import {
  MatSnackBar,
  MatSnackBarModule
} from '@angular/material/snack-bar';


import { ProductFormComponent }
  from '../../../components/product-form/product-form.component';

import { ProductService }
  from '../../services/product.service';
import { PageShellComponent } from '../../../../../../../../shared/layout/page-shell/page-shell.component';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';

@Component({
  selector: 'app-product-create',
  standalone: true,
  imports: [
    CommonModule,
    PageShellComponent,
    ProductFormComponent,
    MatSnackBarModule,
    MatProgressSpinnerModule
  ],
  template: `
    <app-page-shell>
      <div page-content>

        <div
          *ngIf="saving"
          class="save-overlay">
          <mat-spinner diameter="56"></mat-spinner>
          <div class="save-text">
            Creating product...
          </div>
        </div>

        <app-product-form
          [editMode]="false"
          (submitForm)="handleSubmit($event)"
          (cancel)="cancel()">
        </app-product-form>

      </div>
    </app-page-shell>
    `,
  styles: [`
    .save-overlay{
      position:fixed;
      inset:0;
      z-index:9999;
      display:flex;
      flex-direction:column;
      align-items:center;
      justify-content:center;
      gap:16px;
      background:rgba(0,0,0,.35);
      backdrop-filter:blur(4px);
    }
    .save-text{
      color:white;
      font-weight:600;
      font-size:.95rem;
    }
    `]
})
export class ProductCreateComponent {

  saving = false;

  constructor(
    private productService: ProductService,
    private snackbar: MatSnackBar,
    private router: Router
  ) { }

  handleSubmit(formData: FormData) {

    if (this.saving) {
      return;
    }

    this.saving = true;

    this.productService.create(formData)
      .subscribe({

        next: product => {

          this.saving = false;

          this.snackbar.open(
            'Product created successfully',
            'Close',
            {
              duration: 3000
            }
          );

          this.router.navigate([
            '/app/stock',
            product.id
          ]);
        },

        error: () => {

          this.saving = false;

          this.snackbar.open(
            'Create failed',
            'Close',
            {
              duration: 3000
            }
          );
        }
      });
  }

  cancel() {

    this.router.navigate([
      '/app/stock'
    ]);
  }
}