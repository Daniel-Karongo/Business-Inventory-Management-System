import { CommonModule } from '@angular/common';
import {
  Component,
  OnInit
} from '@angular/core';

import {
  ActivatedRoute,
  Router
} from '@angular/router';

import {
  MatSnackBar,
  MatSnackBarModule
} from '@angular/material/snack-bar';

import { MatTabsModule }
  from '@angular/material/tabs';

import { ProductFormComponent }
  from '../../../components/product-form/product-form.component';

import { ProductService }
  from '../../services/product.service';

import { ProductImageAdapter }
  from '../../services/product-image.adapter';

import { EntityImageManagerComponent }
  from '../../../../../../../../shared/components/entity-image-manager/entity-image-manager.component';

import { PageShellComponent }
  from '../../../../../../../../shared/layout/page-shell/page-shell.component';

@Component({
  selector: 'app-product-edit',
  standalone: true,
  imports: [
    CommonModule,
    PageShellComponent,
    ProductFormComponent,
    EntityImageManagerComponent,
    MatTabsModule,
    MatSnackBarModule
  ],
  template: `
<app-page-shell>

  <div page-content>

    <section
      class="edit-workspace"
      *ngIf="!loading">

      <mat-tab-group
        class="product-tabs"
        animationDuration="150ms"
        dynamicHeight>

        <!-- ===================================== -->
        <!-- PRODUCT -->
        <!-- ===================================== -->

        <mat-tab label="Product">

          <div class="tab-pane">

            <div
              class="warning-banner"
              *ngIf="product?.deleted">

              This product is deleted and cannot be edited.

            </div>

            <app-product-form
              [initialValue]="product"
              [editMode]="true"
              (submitForm)="handleSubmit($event)"
              (cancel)="cancel()">

            </app-product-form>

          </div>

        </mat-tab>

        <!-- ===================================== -->
        <!-- DOCUMENTS -->
        <!-- ===================================== -->

        <mat-tab label="Documents">

          <div class="tab-pane">

            <div class="documents-panel">

              <div class="documents-header">

                <h3>
                  Documents & Images
                </h3>

                <p>
                  Manage uploaded files,
                  image lifecycle,
                  audits and restores.
                </p>

              </div>

              <app-entity-image-manager
                *ngIf="product?.id"
                [entityId]="product.id"
                [adapter]="imageAdapter"
                [allowHardDelete]="true">

              </app-entity-image-manager>

            </div>

          </div>

        </mat-tab>

      </mat-tab-group>

    </section>

    <div
      *ngIf="loading"
      class="loading-state">

      Loading product...

    </div>

  </div>

</app-page-shell>
`,
  styles: [`

:host {
  display:block;
}

[page-content] {
  padding-bottom:40px;
}

.edit-workspace {
  margin-top:8px;
}

.product-tabs {
  background:transparent;
}

.tab-pane {
  padding:20px 0 8px;
}

.documents-panel {

  padding:24px;

  border-radius:16px;

  border:1px solid var(--border);

  background:var(--surface);
}

.documents-header {
  margin-bottom:20px;
}

.documents-header h3 {
  margin:0;
  font-size:.95rem;
  font-weight:600;
}

.documents-header p {
  margin:6px 0 0;
  color:var(--text-secondary);
  font-size:.82rem;
}

.warning-banner {

  margin-bottom:16px;

  padding:14px;

  border-radius:10px;

  background:rgba(245,158,11,.10);

  border:1px solid rgba(245,158,11,.25);

  color:#b45309;
}

.loading-state {

  display:flex;

  justify-content:center;

  align-items:center;

  min-height:300px;

  color:var(--text-secondary);
}

::ng-deep .mat-mdc-tab-header {

  border-bottom:1px solid var(--border);

  margin-bottom:8px;
}

::ng-deep .mat-mdc-tab {
  min-width:120px;
}

::ng-deep .mdc-tab__text-label {

  font-size:.88rem;

  font-weight:500;
}

@media(max-width:700px){

  .documents-panel {
    padding:16px;
  }

  .tab-pane {
    padding-top:14px;
  }

}
`]
})
export class ProductEditComponent
  implements OnInit {

  productId!: string;

  product: any;

  loading = true;

  saving = false;

  imageAdapter!: ReturnType<
    typeof ProductImageAdapter
  >;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private productService: ProductService,
    private snackbar: MatSnackBar
  ) { }

  ngOnInit(): void {

    this.imageAdapter =
      ProductImageAdapter(
        this.productService
      );

    const id =
      this.route.snapshot.paramMap.get('id');

    if (!id) {

      this.router.navigate([
        '/app/stock'
      ]);

      return;
    }

    this.productId = id;

    this.loadProduct();
  }

  private loadProduct() {

    const deleted =
      history.state?.deleted === true;

    this.productService
      .getById(
        this.productId,
        deleted
      )
      .subscribe({

        next: product => {

          this.product = product;

          this.loading = false;
        },

        error: () => {

          this.router.navigate([
            '/app/products'
          ]);
        }
      });
  }

  handleSubmit(
    formData: FormData
  ) {

    if (
      this.saving ||
      this.product?.deleted
    ) {
      return;
    }

    this.saving = true;

    this.productService
      .update(
        this.productId,
        formData as any
      )
      .subscribe({

        next: () => {

          this.saving = false;

          this.snackbar.open(
            'Product updated',
            'Close',
            {
              duration: 3000
            }
          );

          this.loadProduct();
        },

        error: () => {

          this.saving = false;

          this.snackbar.open(
            'Update failed',
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
      '/app/products',
      this.productId
    ]);
  }
}