import {
  Component,
  OnInit
} from '@angular/core';

import { CommonModule } from '@angular/common';

import { Router } from '@angular/router';

import { finalize, forkJoin } from 'rxjs';

import {
  WorkflowShellComponent
} from '../../../../../../shared/layout/workflow-shell/workflow-shell.component';

import {
  WorkflowCardComponent
} from '../../../../../../shared/layout/workflow-card/workflow-card.component';

import {
  BranchFormComponent
} from '../../components/branch-form/branch-form.component';

import { BranchService } from '../../services/branch.service';

import {
  BranchFormDTO
} from '../../models/branch.model';

import {
  DepartmentDTO
} from '../../../departments/models/department.model';

import {
  MinimalUserDTO
} from '../../../users/models/user.model';

import { DepartmentService } from '../../../departments/services/department.service';

import { UserService } from '../../../users/services/user/user.service';
import { MatSnackBar } from '@angular/material/snack-bar';

@Component({
  standalone: true,
  selector: 'app-branch-create',
  imports: [
    CommonModule,
    WorkflowShellComponent,
    WorkflowCardComponent,
    BranchFormComponent
  ],
  templateUrl: './branch-create.component.html',
  styleUrls: ['./branch-create.component.scss']
})
export class BranchCreateComponent implements OnInit {

  loading = false;

  users: MinimalUserDTO[] = [];

  departments: DepartmentDTO[] = [];

  userIds: string[] = [];

  departmentIds: string[] = [];

  constructor(
    private router: Router,
    private branchService: BranchService,
    private departmentService: DepartmentService,
    private userService: UserService,
    private snackbar: MatSnackBar
  ) { }

  ngOnInit() {

    forkJoin({
      users:
        this.userService.list(0, 500),

      departments:
        this.departmentService.getAll(false)
    })
      .subscribe({
        next: result => {

          this.users =
            result.users.data;

          this.departments =
            result.departments;
        }
      });
  }

  save(dto: BranchFormDTO) {

    this.loading = true;

    this.branchService
      .create({
        ...dto,
        userIds: this.userIds,
        departmentIds: this.departmentIds
      })
      .pipe(
        finalize(() => {
          this.loading = false;
        })
      )
      .subscribe({
        next: () => {

          this.snackbar.open(
            'Branch created successfully',
            'Close',
            {
              duration: 3000
            }
          );

          this.router.navigate([
            '/app/branches'
          ]);
        },

        error: (err) => {

          const message =
            err?.error?.message
            || 'Failed to create branch';

          this.snackbar.open(
            message,
            'Close',
            {
              duration: 4000
            }
          );
        }
      });
  }
}