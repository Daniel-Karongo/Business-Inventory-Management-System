import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { MatTableModule } from '@angular/material/table';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { MatDialog } from '@angular/material/dialog';

import { DepartmentService } from '../../services/department.service';
import { DepartmentDTO } from '../../models/department.model';
import { ConfirmDialogComponent } from '../../../../shared/components/confirm-dialog/confirm-dialog.component';

@Component({
  standalone: true,
  selector: 'app-department-list',
  imports: [
    CommonModule,
    RouterModule,
    MatTableModule,
    MatIconModule,
    MatButtonModule
  ],
  templateUrl: './department-list.component.html',
  styleUrls: ['./department-list.component.scss']
})
export class DepartmentListComponent implements OnInit {

  displayedColumns = ['name', 'branches', 'rollcall', 'actions'];
  departments: DepartmentDTO[] = [];

  constructor(
    private service: DepartmentService,
    private dialog: MatDialog
  ) { }

  ngOnInit() {
    this.load();
  }

  load() {
    this.service.getAll(false).subscribe(d => this.departments = d);
  }

  delete(dep: DepartmentDTO) {
    if (!dep.id) return;

    const ref = this.dialog.open(ConfirmDialogComponent, {
      data: {
        title: 'Delete Department',
        message: `Delete ${dep.name}?`
      }
    });

    ref.afterClosed().subscribe(ok => {
      if (!ok) return;
      this.service.delete(dep.id, true).subscribe(() => this.load());
    });
  }

  branchNames(d: any): string {
    if (!d.branches || d.branches.length === 0) {
      return '—';
    }
    return d.branches.map((b: any) => b.name).join(', ');
  }
}