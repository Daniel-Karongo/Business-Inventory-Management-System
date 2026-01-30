import { Component, Input, OnChanges, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatIconModule } from '@angular/material/icon';

import { HttpClient } from '@angular/common/http';
import { environment } from '../../../../../../../environments/environment';

@Component({
  selector: 'app-acl-audit-timeline',
  standalone: true,
  imports: [CommonModule, MatIconModule],
  templateUrl: './acl-audit-timeline.component.html',
  styleUrls: ['./acl-audit-timeline.component.scss']
})
export class AclAuditTimelineComponent implements OnChanges {

  private http = inject(HttpClient);

  @Input() entityType!: string;
  @Input() entityId!: string;

  loading = false;
  audits: any[] = [];

  ngOnChanges() {
    if (!this.entityType || !this.entityId) return;
    this.load();
  }

  load() {
    this.loading = true;

    this.http.get<any[]>(
      `${environment.apiUrl}/admin/acl/audits/${this.entityType}/${this.entityId}`
    ).subscribe({
      next: data => {
        this.audits = data ?? [];
        this.loading = false;
      },
      error: () => this.loading = false
    });
  }

  parse(json?: string) {
    if (!json) return null;
    if (json.includes('serialization_failed')) return '⚠ Serialization failed';

    try {
      return JSON.stringify(JSON.parse(json), null, 2);
    } catch {
      return '⚠ Invalid JSON';
    }
  }

  badgeClass(action: string) {
    return action.toLowerCase();
  }
}