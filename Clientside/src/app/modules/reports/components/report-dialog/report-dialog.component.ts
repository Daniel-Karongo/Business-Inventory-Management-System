import { Component, Inject, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogModule } from '@angular/material/dialog';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatNativeDateModule } from '@angular/material/core';

import { ReportMetadata, ReportParameter } from '../../models/report-metadata.model';
import { ReportsService } from '../../services/reports.service';
import { AuthService } from '../../../auth/services/auth.service';
import { AccountsService } from '../../../accounts/services/accounts.service';
import { BranchService } from '../../../branches/services/branch.service';
import { CustomerService } from '../../../customers/services/customer.service';
import { SupplierService } from '../../../suppliers/services/supplier.service';
import { ProductService } from '../../../products/parent/services/product.service';
import { CategoryService } from '../../../categories/services/category.service';
import { ProductVariantService } from '../../../products/variant/services/product-variant.service';
import { MatDialog } from '@angular/material/dialog';
import { ReportErrorDialogComponent } from '../report-error-dialog/report-error-dialog.component';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';


@Component({
  templateUrl: './report-dialog.component.html',
  styleUrls: ['./report-dialog.component.scss'],
  imports: [
    CommonModule,
    FormsModule,
    MatDialogModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatButtonModule,
    MatDatepickerModule,
    MatNativeDateModule,
    MatCheckboxModule,
    MatProgressSpinnerModule
  ]
})
export class ReportDialogComponent implements OnInit {

  params: Record<string, any> = {};
  format: 'PDF' | 'XLSX' | 'CSV' = 'PDF';
  loading = false;

  accounts: any[] = [];
  branches: any[] = [];
  customers: any[] = [];
  suppliers: any[] = [];
  products: any[] = [];
  categories: any[] = [];
  variants: any[] = [];

  orderedParams: ReportParameter[] = [];

  private DEFAULT_FROM = new Date(2025, 0, 1); // 01/01/2025
  private DEFAULT_TO = new Date(2027, 0, 1); // 01/01/2027

  consolidated = false;

  constructor(
    @Inject(MAT_DIALOG_DATA) public report: ReportMetadata,

    private dialog: MatDialog,

    private customerSvc: CustomerService,
    private supplierSvc: SupplierService,
    private productSvc: ProductService,
    private categorySvc: CategoryService,
    private variantSvc: ProductVariantService,

    private reports: ReportsService,
    private auth: AuthService,
    private accountsSvc: AccountsService,
    private branchesSvc: BranchService,
    private snack: MatSnackBar
  ) { }

  ngOnInit(): void {
    const me = this.auth.getSnapshot();

    // ---- ORDER PARAMETERS ----
    this.orderedParams = [...this.report.parameters].sort((a, b) => {
      const order = (p: ReportParameter) => {
        if (p.name === 'FROM_DATE') return 1;
        if (p.name === 'START_DATE') return 2;
        if (p.name === 'TO_DATE') return 3;
        if (p.name === 'END_DATE') return 4;
        if (p.type === 'DATE') return 5;
        if (p.type === 'BRANCH') return 6;
        if (p.type === 'ACCOUNT') return 8;
        return 10;
      };
      return order(a) - order(b);
    });

    // ---- INITIALISE VALUES ----
    for (const p of this.orderedParams) {

      if (p.type === 'DATE') {
        if (p.name === 'FROM_DATE' || p.name == 'START_DATE') {
          this.params[p.name] = new Date(this.DEFAULT_FROM);
        } else if (p.name === 'TO_DATE' || p.name == 'END_DATE') {
          this.params[p.name] = new Date(this.DEFAULT_TO);
        } else {
          this.params[p.name] = new Date();
        }
      }

      if (p.type === 'BRANCH') {
        this.params[p.name] = me?.branchId;
        this.branchesSvc.getAll().subscribe(b => this.branches = b);
      }

      if (p.type === 'ACCOUNT') {
        this.accountsSvc.list().subscribe(a => this.accounts = a);
      }

      if (p.type === 'CUSTOMER') {
        this.customerSvc.list(0, 1000).subscribe(r => {
          this.customers = r.data ?? r;
        });
      }

      if (p.type === 'SUPPLIER') {
        this.supplierSvc.getAll().subscribe(s => this.suppliers = s);
      }

      if (p.type === 'PRODUCT') {
        this.productSvc.getAll(false).subscribe(p => this.products = p);
      }

      if (p.type === 'CATEGORY') {
        this.categorySvc.getAll('flat').subscribe(c => this.categories = c);
      }

      if (p.type === 'VARIANT') {
        // variants are loaded lazily when product changes
        this.variants = [];
      }
    }
  }

  /** yyyy-MM-dd for backend (SAFE) */
  private toIsoSafe(value: any): string | null {
    if (!(value instanceof Date)) return null;

    const y = value.getFullYear();
    const m = String(value.getMonth() + 1).padStart(2, '0');
    const d = String(value.getDate()).padStart(2, '0');

    return `${y}-${m}-${d}`;
  }

  labelFor(param: ReportParameter): string {
    const name = param.name
      .replace(/_ID$/, '')   // remove ID
      .replace(/_/g, ' ')
      .toLowerCase()
      .replace(/\b\w/g, c => c.toUpperCase());

    return name;
  }

  onProductChange(productId: string) {
    if (!productId) {
      this.variants = [];
      this.params['VARIANT_ID'] = null;
      return;
    }

    this.variantSvc.forProduct(productId).subscribe(v => {
      this.variants = v;
      this.params['VARIANT_ID'] = null;
    });
  }

  isMissingRequired(): boolean {
    return this.orderedParams.some(p =>
      p.required &&
      (this.params[p.name] === undefined ||
        this.params[p.name] === null ||
        this.params[p.name] === '')
    );
  }

  generate() {
    this.loading = true;

    const parameters: any = {};

    for (const p of this.orderedParams) {
      if (p.type === 'DATE') {
        parameters[p.name] = this.toIsoSafe(this.params[p.name]);
      } else {
        parameters[p.name] = this.params[p.name];
      }
    }

    // 🔥 IMPORTANT: Remove branch when consolidated
    if (this.consolidated) {
      delete parameters['BRANCH_ID'];
    }

    const finalReportKey = this.report.key;

    this.reports.generate(
      finalReportKey,
      parameters,
      this.format
    ).subscribe({
      next: blob => {
        this.handleDownload(blob, finalReportKey);
        this.loading = false;
      },
      error: async err => {
        this.loading = false;

        let message = 'Failed to generate report';

        if (err?.error instanceof Blob) {
          const text = await err.error.text();
          message = text || message;
        }

        this.dialog.open(ReportErrorDialogComponent, {
          width: '380px',
          data: { message }
        });
      }
    });
  }

  private handleDownload(blob: Blob, reportKey: string) {

    const extension =
      this.format === 'PDF' ? 'pdf' :
        this.format === 'XLSX' ? 'xlsx' :
          'csv';

    const fileName = `${reportKey}_${new Date().getTime()}.${extension}`;

    if (this.format === 'PDF') {
      const url = URL.createObjectURL(blob);
      window.open(url, '_blank');
      return;
    }

    // Excel / CSV
    const link = document.createElement('a');
    link.href = URL.createObjectURL(blob);
    link.download = fileName;
    link.click();
    URL.revokeObjectURL(link.href);
  }
}