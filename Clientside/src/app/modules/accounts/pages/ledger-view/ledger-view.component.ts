import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ActivatedRoute, Router } from '@angular/router';

import { MatTableModule } from '@angular/material/table';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';

import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../../environments/environment';

@Component({
  selector: 'app-ledger-view',
  standalone: true,
  imports: [
    CommonModule,
    MatTableModule,
    MatIconModule,
    MatButtonModule
  ],
  templateUrl: './ledger-view.component.html',
  styleUrls: ['./ledger-view.component.scss']
})
export class LedgerViewComponent implements OnInit {

  accountId!: string;
  account: any;
  rows: any[] = [];

  displayedColumns = [
    'date',
    'reference',
    'direction',
    'amount',
    'balance'
  ];

  loading = true;

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private http: HttpClient
  ) {}

  ngOnInit(): void {
    const id = this.route.snapshot.paramMap.get('accountId');
    if (!id) return;

    this.accountId = id;

    this.http.get<any>(`${environment.apiUrl}/accounts/${id}`)
      .subscribe(a => this.account = a);

    this.http.get<any[]>(`${environment.apiUrl}/accounting/ledger/${id}`)
      .subscribe({
        next: r => {
          this.rows = r;
          this.loading = false;
        },
        error: () => this.loading = false
      });
  }

  back() {
    this.router.navigate(['/accounts/ledger']);
  }
}
