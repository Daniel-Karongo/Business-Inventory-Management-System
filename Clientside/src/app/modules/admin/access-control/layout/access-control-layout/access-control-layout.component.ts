import { Component } from '@angular/core';
import { RouterOutlet, RouterModule } from '@angular/router';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-access-control-layout',
  standalone: true,
  imports: [
    CommonModule,
    RouterOutlet,
    RouterModule   // ✅ REQUIRED
  ],
  templateUrl: './access-control-layout.component.html',
  styleUrls: ['./access-control-layout.component.scss']
})
export class AccessControlLayoutComponent {}