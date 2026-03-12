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

  constructor(private userService: UserService) {}

  ngOnInit() {
    if (!this.userId) return;

    this.userService.auditsForUser(this.userId).subscribe(a =>
      this.audits = a || []
    );
  }
}