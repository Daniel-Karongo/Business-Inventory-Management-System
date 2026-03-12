import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatCardModule } from '@angular/material/card';
import { Router } from '@angular/router';

@Component({
  standalone: true,
  selector: 'app-tax-dashboard',
  imports: [CommonModule, MatCardModule],
  templateUrl: './tax-dashboard.component.html',
  styleUrls: ['./tax-dashboard.component.scss']
})
export class TaxDashboardComponent {

  constructor(private router: Router) {}

  goVat() { this.router.navigate(['/finance/tax/vat']); }
  goCorporate() { this.router.navigate(['/finance/tax/corporate']); }
}