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
import { BranchBulkImportDialogComponent } from '../../components/branch-bulk-import-dialog/branch-bulk-import-dialog.component';

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

  displayedColumns = ['code', 'name', 'location', 'createdAt', 'actions'];
  branches: BranchDTO[] = [];

  constructor(
    private branchService: BranchService,
    private dialog: MatDialog
  ) { }

  ngOnInit() {
    this.load();
  }

  load() {
    this.branchService.getAll(false).subscribe(b => this.branches = b);
  }

  openBulkImport() {
    this.dialog.open(BranchBulkImportDialogComponent, {
      width: '1100px',
      maxWidth: '95vw',
      maxHeight: '90vh'
    }).afterClosed().subscribe(imported => {
      if (imported === true) {
        // 🔄 REFRESH LIST
        this.load();
      }
    });
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