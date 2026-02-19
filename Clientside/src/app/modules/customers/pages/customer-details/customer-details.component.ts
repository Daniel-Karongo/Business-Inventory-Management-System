import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { CommonModule } from '@angular/common';

import { MatTabsModule } from '@angular/material/tabs';
import { MatIconModule } from '@angular/material/icon';
import { MatTableModule } from '@angular/material/table';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatButtonModule } from '@angular/material/button';

import { CustomerService } from '../../services/customer.service';
import { SmsDialogComponent } from '../../../../shared/components/sms-dialog/sms-dialog.component';
import { SmsService } from '../../../communication/services/sms.service';
import { EntityActionService } from '../../../../shared/services/entity-action.service';

@Component({
  standalone: true,
  selector: 'app-customer-details',
  imports: [
    CommonModule,
    MatTabsModule,
    MatIconModule,
    MatTableModule,
    MatDialogModule,
    MatButtonModule
  ],
  templateUrl: './customer-details.component.html',
  styleUrls: ['./customer-details.component.scss']
})
export class CustomerDetailsComponent implements OnInit {

  id!: string;

  customer: any;
  payments: any[] = [];
  sales: any[] = [];

  paymentColumns = ['date', 'amount', 'note'];
  saleColumns = ['date', 'total', 'status'];

  loading = true;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private service: CustomerService,
    private entityAction: EntityActionService,
    private smsService: SmsService,
    private dialog: MatDialog,
    private snackbar: MatSnackBar
  ) { }

  ngOnInit(): void {
    this.id = this.route.snapshot.paramMap.get('id')!;
    this.load();
  }

  load(): void {
    this.service.get(this.id).subscribe(c => this.customer = c);
    this.service.payments(this.id).subscribe(p => this.payments = p || []);
    this.service.sales(this.id).subscribe(s => this.sales = s || []);
    this.loading = false;
  }

  /* ================= ACTIONS ================= */

  edit(): void {
    this.router.navigate(['/customers', this.id, 'edit']);
  }

  toggleActive(): void {

    if (!this.customer) return;

    this.entityAction.toggleSingle(this.customer, {
      entityName: 'Customer',
      displayName: (c) => c.name,

      disableReasons: [],
      restoreReasons: [],
      deleteReasons: [],

      disable: (id, reason) =>
        this.service.softDelete(id, reason),

      restore: (id, reason) =>
        this.service.restore(id, reason),

      hardDelete: (id, reason) =>
        this.service.hardDelete(id, reason),

      reload: () => this.load()
    });

  }

  sendSms(): void {
    if (!this.customer?.phoneNumbers?.length) return;

    const ref = this.dialog.open(SmsDialogComponent, {
      maxWidth: '95vw',
      data: { customers: [this.customer] }
    });

    ref.afterClosed().subscribe(res => {
      if (!res) return;
      this.smsService.sendToCustomers(res.customerIds, res.message).subscribe(() => {
        this.snackbar.open('SMS sent', 'Close', { duration: 2000 });
      });
    });
  }
}