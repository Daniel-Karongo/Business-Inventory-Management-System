import { Component, Input, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatIconModule } from '@angular/material/icon';
import { UserService } from '../../services/user/user.service';

@Component({
  selector: 'app-user-audit-timeline',
  standalone: true,
  imports: [CommonModule, MatIconModule],
  templateUrl: './user-audit-timeline.component.html',
  styleUrls: ['./user-audit-timeline.component.scss']
})
export class UserAuditTimelineComponent implements OnInit {

  @Input() userId!: string;
  audits: any[] = [];

  constructor(private userService: UserService) { }

  ngOnInit() {
    if (!this.userId) return;

    this.userService.auditsForUser(this.userId).subscribe(a =>
      this.audits = a || []
    );
  }

  displayAction(action: string): string {
    return USER_ACTION_LABELS[action] ??
      action.toLowerCase().replace(/_/g, ' ')
        .replace(/\b\w/g, c => c.toUpperCase());
  }

  getIcon(action: string): string {
    switch (action) {
      case 'CREATE': return 'person_add';
      case 'UPDATE': return 'edit';
      case 'SOFT_DELETE': return 'delete';
      case 'HARD_DELETE': return 'delete_forever';
      case 'RESTORE': return 'restore';
      case 'FORCED_LOGOUT_ALL': return 'logout';
      case 'PASSWORD_CHANGE_BLOCKED': return 'lock';
      default: return 'history';
    }
  }
}

export type UserAuditAction =
  | 'CREATE'
  | 'UPDATE'
  | 'SOFT_DELETE'
  | 'RESTORE'
  | 'HARD_DELETE'
  | 'PASSWORD_CHANGE_BLOCKED'
  | 'FORCED_LOGOUT_ALL';

const USER_ACTION_LABELS: Record<string, string> = {
  CREATE: 'User Created',
  UPDATE: 'Updated',
  SOFT_DELETE: 'Soft Deleted',
  RESTORE: 'Restored',
  HARD_DELETE: 'Permanently Deleted',
  PASSWORD_CHANGE_BLOCKED: 'Blocked Password Change',
  FORCED_LOGOUT_ALL: 'Forced Logout (Security)'
};