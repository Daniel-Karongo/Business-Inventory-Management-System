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

import { BranchService } from '../../services/branch.service';
import { DepartmentService } from '../../../departments/services/department.service';
import { UserService } from '../../../users/services/user/user.service';

import { BranchDTO } from '../../models/branch.model';
import { DepartmentDTO } from '../../../departments/models/department.model';
import { MinimalUserDTO } from '../../../users/models/user.model';

import { SearchableAssignComponent } from '../../../../shared/components/searchable-assign/searchable-assign.component';

@Component({
  standalone: true,
  selector: 'app-branch-edit',
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
  templateUrl: './branch-edit.component.html',
  styleUrls: ['./branch-edit.component.scss']
})
export class BranchEditComponent implements OnInit {

  id!: string;
  fg!: FormGroup;

  users: MinimalUserDTO[] = [];
  departments: DepartmentDTO[] = [];

  userIds: string[] = [];
  departmentIds: string[] = [];

  filteredUsers$!: Observable<MinimalUserDTO[]>;
  filteredDepts$!: Observable<DepartmentDTO[]>;

  constructor(
    private fb: FormBuilder,
    private route: ActivatedRoute,
    private router: Router,
    private branchService: BranchService,
    private deptService: DepartmentService,
    private userService: UserService
  ) { }

  ngOnInit() {
    this.fg = this.fb.group({
      branchCode: ['', Validators.required],
      name: ['', Validators.required],
      location: [''],
      phone: [''],
      email: ['']
    });

    this.id = this.route.snapshot.paramMap.get('id')!;

    // ---- USERS ----
    this.userService.list(0, 500).subscribe(r => {
      this.users = r.data;
      this.filteredUsers$ = this.userService.list(0, 500).pipe(
        map(x => x.data)
      );
    });

    // ---- DEPARTMENTS ----
    this.deptService.getAll(false).subscribe(d => {
      this.departments = d;
      this.filteredDepts$ = this.deptService.getAll(false);
    });

    // ---- BRANCH ----
    this.branchService.getById(this.id).subscribe(b => this.loadBranch(b));
  }

  private loadBranch(b: BranchDTO) {
    this.fg.patchValue(b);
    this.userIds = b.users?.map(u => u.id) ?? [];
    this.departmentIds = b.departments?.map(d => d.id) ?? [];
  }

  // ---- REQUIRED by SearchableAssignComponent ----
  userDisplay = (u: MinimalUserDTO) => u.username;
  userId = (u: MinimalUserDTO) => u.id;

  deptDisplay = (d: DepartmentDTO) => d.name;
  deptId = (d: DepartmentDTO) => d.id!;

  save() {
    if (this.fg.invalid) return;

    this.branchService.update(this.id, {
      ...this.fg.value,
      userIds: this.userIds,
      departmentIds: this.departmentIds
    }).subscribe(() =>
      this.router.navigate(['/branches', this.id])
    );
  }
}