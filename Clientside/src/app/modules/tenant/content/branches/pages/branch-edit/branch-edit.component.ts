import {
  Component,
  OnInit
} from '@angular/core';

import { CommonModule } from '@angular/common';

import {
  ActivatedRoute,
  Router
} from '@angular/router';

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
  BranchDetailsDTO,
  BranchFormDTO
} from '../../models/branch.model';

import {
  MinimalUserDTO
} from '../../../users/models/user.model';

import {
  UserService
} from '../../../users/services/user/user.service';
import { MatSnackBar } from '@angular/material/snack-bar';

@Component({
  standalone: true,
  selector: 'app-branch-edit',
  imports: [
    CommonModule,
    WorkflowShellComponent,
    WorkflowCardComponent,
    BranchFormComponent
  ],
  templateUrl: './branch-edit.component.html',
  styleUrls: ['./branch-edit.component.scss']
})
export class BranchEditComponent implements OnInit {

  id!: string;

  loading = true;

  saving = false;

  branch?: BranchDetailsDTO;

  users: MinimalUserDTO[] = [];

  userIds: string[] = [];

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private branchService: BranchService,
    private userService: UserService,
    private snackbar: MatSnackBar
  ) { }

  ngOnInit() {

    this.id =
      this.route.snapshot.paramMap.get('id')!;

    forkJoin({
      branch:
        this.branchService.getById(this.id),

      users:
        this.userService.list(0, 500),
    })
      .subscribe({
        next: result => {

          this.branch =
            result.branch;

          this.users =
            result.users.data;

          this.userIds =
            result.branch.users?.map(u => u.id) ?? [];

          this.loading = false;
        },

        error: () => {
          this.loading = false;
        }
      });
  }

  save(dto: BranchFormDTO) {

    this.saving = true;

    this.branchService
      .update(
        this.id,
        {
          ...dto,
          userIds: this.userIds
        }
      )
      .pipe(
        finalize(() => {
          this.saving = false;
        })
      )
      .subscribe({
        next: () => {

          this.snackbar.open(
            'Branch updated successfully',
            'Close',
            {
              duration: 3000
            }
          );

          this.router.navigate([
            '/app/branches',
            this.id,
            'overview'
          ]);
        },

        error: (err) => {

          const message =
            err?.error?.message
            || 'Failed to update branch';

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