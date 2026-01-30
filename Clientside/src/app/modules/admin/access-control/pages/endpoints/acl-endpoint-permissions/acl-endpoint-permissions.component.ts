import { Component, OnInit, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';

import { AclEndpointPermissionsService } from './acl-endpoint-permissions.service';

@Component({
  selector: 'app-acl-endpoint-permissions',
  standalone: true,
  imports: [CommonModule, MatSelectModule, MatButtonModule],
  templateUrl: './acl-endpoint-permissions.component.html',
  styleUrls: ['./acl-endpoint-permissions.component.scss']
})
export class AclEndpointPermissionsComponent implements OnInit {

  private svc = inject(AclEndpointPermissionsService);

  endpoints: any[] = [];
  permissions: any[] = [];

  loadingEndpoints = true;
  loadingPermissions = true;

  ngOnInit() {
    this.load();
  }

  load() {
    this.loadingEndpoints = true;
    this.loadingPermissions = true;

    this.svc.endpoints().subscribe(e => {
      this.endpoints = e;
      this.loadingEndpoints = false;
    });

    this.svc.permissions().subscribe(p => {
      this.permissions = p.sort((a, b) =>
        a.code.localeCompare(b.code)
      );
      this.loadingPermissions = false;
    });
  }

  changePermission(ep: any, permissionId: string) {
    this.svc.remove(ep.id).subscribe(() => {
      this.svc.reassign({
        endpointId: ep.id,
        permissionId
      }).subscribe(() => this.load());
    });
  }
}