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
  MinimalUserDTO
} from '../../../users/models/user.model';

import { UserService } from '../../../users/services/user/user.service';
import { MatSnackBar } from '@angular/material/snack-bar';
import { BranchContextService } from '../../../../../../core/services/branch-context.service';

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

  userIds: string[] = [];

  constructor(
    private router: Router,
    private branchService: BranchService,
    private userService: UserService,
    private snackbar: MatSnackBar,
    private branchContext: BranchContextService
  ) { }

  ngOnInit() {

    forkJoin({
      users:
        this.userService.list(0, 500)
    })
      .subscribe({
        next: result => {

          this.users =
            result.users.data;
        }
      });
  }

  save(dto: BranchFormDTO) {

    this.loading = true;

    this.branchService
      .create({
        ...dto,
        userIds: this.userIds
      })
      .pipe(
        finalize(() => {
          this.loading = false;
        })
      )
      .subscribe({
        next: () => {

          this.branchContext
            .refreshBranches();

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