import { Component, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';

import { MatIconModule } from '@angular/material/icon';
import { MatTooltipModule } from '@angular/material/tooltip';

import { SidebarService } from '../../../../core/services/sidebar.service';
import { PLATFORM_NAV_ITEMS } from '../platform-nav.config';
import { TenantBrandingService } from '../../../../core/services/tenant-branding.service';

@Component({
  selector: 'app-platform-sidebar',
  standalone: true,
  imports: [
    CommonModule,
    RouterModule,
    MatIconModule,
    MatTooltipModule
  ],
  templateUrl: './platform-sidebar.component.html',
  styleUrls: ['./platform-sidebar.component.scss']
})
export class PlatformSidebarComponent {
  
  private branding = inject(TenantBrandingService);

  logo$ = this.branding.logo$;
  items = PLATFORM_NAV_ITEMS;

  constructor(
    public sidebar: SidebarService
  ) { }

  toggleSidebar() {
    this.sidebar.toggle();
  }

}