import {
  CommonModule
} from '@angular/common';

import {
  Component,
  DestroyRef,
  OnInit,
  inject
} from '@angular/core';

import {
  takeUntilDestroyed
} from '@angular/core/rxjs-interop';

import {
  ActivatedRoute,
  RouterModule
} from '@angular/router';

import {
  finalize
} from 'rxjs';

import {
  MatButtonModule
} from '@angular/material/button';

import {
  MatIconModule
} from '@angular/material/icon';

import {
  MatSnackBar
} from '@angular/material/snack-bar';

import {
  SalesService
} from '../../services/sales.service';
import { EnterprisePrintService } from '../../../../../../shared/printing/services/enterprise-print.service';
import { SalePrintMapper } from '../../utils/sale-print.mapper';
import { SaleDTO } from '../../../stock/models/sale.model';

@Component({
  standalone: true,
  selector: 'app-sale-receipt',

  imports: [
    CommonModule,
    RouterModule,
    MatButtonModule,
    MatIconModule
  ],

  templateUrl:
    './sale-receipt.component.html',

  styleUrls:
    ['./sale-receipt.component.scss']
})
export class SaleReceiptComponent
  implements OnInit {

  private readonly route =
    inject(ActivatedRoute);

  private readonly destroyRef =
    inject(DestroyRef);

  private readonly snackBar =
    inject(MatSnackBar);

  private readonly salesService =
    inject(SalesService);

  private readonly enterprisePrintService =
    inject(EnterprisePrintService);

  private readonly salePrintMapper =
    inject(SalePrintMapper);

  sale: SaleDTO | null = null;

  loading = false;

  ngOnInit(): void {

    this.route.paramMap
      .pipe(
        takeUntilDestroyed(
          this.destroyRef
        )
      )
      .subscribe(params => {

        const id =
          params.get('id');

        if (!id) {
          return;
        }

        this.load(id);
      });
  }

  load(
    id: string
  ): void {

    this.loading = true;

    this.salesService.get(id)
      .pipe(
        finalize(() => {

          this.loading = false;
        }),

        takeUntilDestroyed(
          this.destroyRef
        )
      )
      .subscribe({

        next: sale => {

          this.sale = sale;
        },

        error: error => {

          this.snackBar.open(
            error?.error?.message
            ?? 'Failed to load receipt.',
            'Close',
            {
              duration: 4000
            }
          );
        }
      });
  }

  async printA4(): Promise<void> {

    if (!this.sale) {
      return;
    }

    const data =
      this.salePrintMapper.map(
        this.sale
      );

    await this.enterprisePrintService
      .printA4(data);
  }

  async printThermal80(): Promise<void> {

    if (!this.sale) {
      return;
    }

    const data =
      this.salePrintMapper.map(
        this.sale
      );

    await this.enterprisePrintService
      .printThermal80(data);
  }

  async printThermal58(): Promise<void> {

    if (!this.sale) {
      return;
    }

    const data =
      this.salePrintMapper.map(
        this.sale
      );

    await this.enterprisePrintService
      .printThermal58(data);
  }
}