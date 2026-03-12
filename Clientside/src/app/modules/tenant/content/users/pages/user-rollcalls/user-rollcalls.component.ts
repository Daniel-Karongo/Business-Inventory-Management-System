import { Component, Input, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatTableModule } from '@angular/material/table';
import { UserService } from '../../services/user/user.service';

@Component({
  selector: 'app-user-rollcalls',
  standalone: true,
  imports: [CommonModule, MatTableModule],
  templateUrl: './user-rollcalls.component.html',
  styleUrls: ['./user-rollcalls.component.scss']
})
export class UserRollcallsComponent implements OnInit {

  @Input() userId!: string;

  displayedColumns = ['time', 'branch', 'department', 'status', 'method'];
  rollcalls: any[] = [];
  DATE_FORMAT = 'dd/MM/yy, hh:mm a';

  constructor(private userService: UserService) {}

  ngOnInit() {
    if (!this.userId) return;

    this.userService.getUserRollcalls(this.userId).subscribe(r =>
      this.rollcalls = r || []
      
    );
    console.log(this.rollcalls);
  }
}