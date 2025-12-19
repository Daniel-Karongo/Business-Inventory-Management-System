import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import {
  FormBuilder,
  Validators,
  ReactiveFormsModule,
  FormGroup
} from '@angular/forms';
import { ActivatedRoute, Router, RouterModule } from '@angular/router';
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
  selector: 'app-department-edit',
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
  templateUrl: './department-edit.component.html',
  styleUrls: ['./department-edit.component.scss']
})
export class DepartmentEditComponent implements OnInit {

  id!: string;
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
    private route: ActivatedRoute,
    private router: Router,
    private deptService: DepartmentService,
    private branchService: BranchService,
    private userService: UserService
  ) { }

  ngOnInit() {
    this.fg = this.fb.group({
      name: ['', Validators.required],
      description: [''],
      rollcallStartTime: [''],
      gracePeriodMinutes: [0]
    });

    this.id = this.route.snapshot.paramMap.get('id')!;

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

    this.deptService.get(this.id).subscribe(d => this.loadDepartment(d));
  }

  private loadDepartment(d: DepartmentDTO) {
    this.fg.patchValue(d);
    this.headIds = d.heads?.map(h => h.id) ?? [];
    this.memberIds = d.members?.map(m => m.id) ?? [];
    this.branchIds = d.branches?.map(b => b.id) ?? [];
  }

  // ---- helpers ----
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

    this.deptService.update(this.id, {
      ...this.fg.value,
      headIds: this.headIds,
      memberIds: this.memberIds,
      branchIds: this.branchIds
    }).subscribe(() =>
      this.router.navigate(['/departments', this.id])
    );
  }
}