import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterOutlet } from '@angular/router';
import { AsyncPipe } from '@angular/common';

import { SidebarService } from '../../../core/services/sidebar.service';

import { BreadcrumbComponent }
  from '../../../shared/components/breadcrumb/breadcrumb.component';
import { PlatformTopbarComponent } from '../navigation/topbar/platform-topbar.component';
import { PlatformSidebarComponent } from '../navigation/sidebar/platform-sidebar.component';

@Component({
  standalone: true,
  selector: 'app-platform-layout',
  templateUrl: './platform-layout.component.html',
  styleUrls: ['./platform-layout.component.scss'],
  imports: [
    CommonModule,
    RouterOutlet,
    AsyncPipe,
    PlatformSidebarComponent,
    PlatformTopbarComponent,
    BreadcrumbComponent
  ]
})
export class PlatformLayoutComponent {

  constructor(
    public sidebar: SidebarService
  ) { }

}