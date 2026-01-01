import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { MatTableModule } from '@angular/material/table';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { MatDialog } from '@angular/material/dialog';

import { BranchService } from '../../services/branch.service';
import { BranchDTO } from '../../models/branch.model';
import { ConfirmDialogComponent } from '../../../../shared/components/confirm-dialog/confirm-dialog.component';
import { MatTooltipModule } from '@angular/material/tooltip';

@Component({
  standalone: true,
  selector: 'app-branch-list',
  imports: [
    CommonModule,
    RouterModule,
    MatTableModule,
    MatIconModule,
    MatButtonModule,
    MatTooltipModule
  ],
  templateUrl: './branch-list.component.html'
})
export class BranchListComponent implements OnInit {

  displayedColumns = ['code', 'name', 'location', 'actions'];
  branches: BranchDTO[] = [];

  constructor(
    private branchService: BranchService,
    private dialog: MatDialog
  ) {}

  ngOnInit() {
    this.load();
  }

  load() {
    this.branchService.getAll(false).subscribe(b => this.branches = b);
  }

  delete(branch: BranchDTO) {
    const ref = this.dialog.open(ConfirmDialogComponent, {
      data: {
        title: 'Delete Branch',
        message: `Delete ${branch.name}?`
      }
    });

    ref.afterClosed().subscribe(ok => {
      if (!ok || !branch.id) return;
      this.branchService.delete(branch.id, true).subscribe(() => this.load());
    });
  }
}