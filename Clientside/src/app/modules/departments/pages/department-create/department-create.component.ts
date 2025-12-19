import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import {
  FormBuilder,
  Validators,
  ReactiveFormsModule,
  FormGroup
} from '@angular/forms';
import { Router, RouterModule } from '@angular/router';
import { Observable, map } from 'rxjs';

import { MatButtonModule } from '@angular/material/button';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatExpansionModule } from '@angular/material/expansion';

import { DepartmentService } from '../../services/department.service';
import { BranchService } from '../../../branches/services/branch.service';
import { UserService } from '../../../users/services/user/user.service';

import { DepartmentDTO } from '../../models/department.model';
import { BranchDTO } from '../../../branches/models/branch.model';
import { MinimalUserDTO } from '../../../users/models/user.model';

import { SearchableAssignComponent } from '../../../../shared/components/searchable-assign/searchable-assign.component';

@Component({
  standalone: true,
  selector: 'app-department-create',
  imports: [
    CommonModule,
    ReactiveFormsModule,
    RouterModule,
    MatButtonModule,
    MatFormFieldModule,
    MatInputModule,
    MatExpansionModule,
    SearchableAssignComponent
  ],
  templateUrl: './department-create.component.html',
  styleUrls: ['./department-create.component.scss']
})
export class DepartmentCreateComponent implements OnInit {

  fg!: FormGroup;

  users: MinimalUserDTO[] = [];
  branches: BranchDTO[] = [];

  headIds: string[] = [];
  memberIds: string[] = [];
  branchIds: string[] = [];

  filteredUsers$!: Observable<MinimalUserDTO[]>;
  filteredBranches$!: Observable<BranchDTO[]>;

  constructor(
    private fb: FormBuilder,
    private deptService: DepartmentService,
    private branchService: BranchService,
    private userService: UserService,
    private router: Router
  ) { }

  ngOnInit() {
    this.fg = this.fb.group({
      name: ['', Validators.required],
      description: [''],
      rollcallStartTime: ['09:00'],
      gracePeriodMinutes: [15]
    });

    this.userService.list(0, 500).subscribe(r => {
      this.users = r.data;
      this.filteredUsers$ = this.userService
        .list(0, 500)
        .pipe(map(x => x.data));
    });

    this.branchService.getAll(false).subscribe(b => {
      this.branches = b;
      this.filteredBranches$ = this.branchService.getAll(false);
    });
  }

  // ---- SearchableAssign helpers ----
  userDisplay = (u: MinimalUserDTO) => u.username;
  userId = (u: MinimalUserDTO) => u.id;

  branchDisplay = (b: BranchDTO) => b.name;
  branchId = (b: BranchDTO) => b.id!;

  private validateNoOverlap(): boolean {
    const overlap = this.headIds.filter(id => this.memberIds.includes(id));
    if (overlap.length) {
      alert('A user cannot be both head and member.');
      return false;
    }
    return true;
  }

  save() {
    if (this.fg.invalid) return;
    if (!this.validateNoOverlap()) return;

    this.deptService.create({
      ...this.fg.value,
      headIds: this.headIds,
      memberIds: this.memberIds,
      branchIds: this.branchIds
    }).subscribe(() => this.router.navigate(['/departments']));
  }
}