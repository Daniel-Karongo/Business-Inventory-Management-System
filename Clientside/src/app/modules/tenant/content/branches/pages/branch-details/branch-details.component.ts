import {
  Component,
  OnInit
} from '@angular/core';

import { CommonModule } from '@angular/common';

import {
  ActivatedRoute,
  RouterModule
} from '@angular/router';

import {
  WorkflowCardComponent
} from '../../../../../../shared/layout/workflow-card/workflow-card.component';

import { BranchService } from '../../services/branch.service';

import {
  BranchDetailsDTO
} from '../../models/branch.model';
import { MatIconModule } from '@angular/material/icon';

@Component({
  standalone: true,
  selector: 'app-branch-details',
  imports: [
    CommonModule,
    RouterModule,
    WorkflowCardComponent,
    MatIconModule
  ],
  templateUrl: './branch-details.component.html',
  styleUrls: ['./branch-details.component.scss']
})
export class BranchDetailsComponent implements OnInit {

  branch?: BranchDetailsDTO;

  loading = true;

  constructor(
    private route: ActivatedRoute,
    private service: BranchService
  ) { }

  ngOnInit(): void {

    const id =
      this.route.parent?.snapshot.paramMap.get('id');

    if (!id) {

      this.loading = false;

      return;
    }

    this.service
      .getById(id)
      .subscribe({
        next: branch => {

          this.branch = branch;

          this.loading = false;
        },

        error: () => {

          this.loading = false;
        }
      });
  }
}