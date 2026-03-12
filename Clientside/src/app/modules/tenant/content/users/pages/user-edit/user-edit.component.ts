import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ActivatedRoute, Router } from '@angular/router';
import {
  FormArray,
  FormBuilder,
  FormGroup,
  Validators,
  ReactiveFormsModule
} from '@angular/forms';

import { MatCardModule } from '@angular/material/card';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatSelectModule } from '@angular/material/select';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';

import { UserService } from '../../services/user/user.service';
import { RoleService } from '../../services/role/role.service';
import { BranchService } from '../../../branches/services/branch.service';
import { DepartmentService } from '../../../departments/services/department.service';

import { User } from '../../models/user.model';
import {
  DepartmentAssignmentDTO,
  DepartmentDTO
} from '../../../departments/models/department.model';
import { BranchHierarchyDTO } from '../../../branches/models/branch.model';

@Component({
  selector: 'app-user-edit',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatCardModule,
    MatFormFieldModule,
    MatSelectModule,
    MatInputModule,
    MatButtonModule,
    MatIconModule,
    MatSnackBarModule
  ],
  templateUrl: './user-edit.component.html',
  styleUrls: ['./user-edit.component.scss']
})
export class UserEditComponent implements OnInit {

  form!: FormGroup;
  originalUsername = '';

  roles: any[] = [];
  branches: any[] = [];
  departments: DepartmentDTO[] = [];

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private fb: FormBuilder,
    private userService: UserService,
    private roleService: RoleService,
    private branchService: BranchService,
    private departmentService: DepartmentService,
    private snackbar: MatSnackBar
  ) {}

  /* ============================================================
     INIT
  ============================================================ */

  ngOnInit(): void {
    const username = this.route.snapshot.paramMap.get('username');
    if (!username) return;

    this.originalUsername = username;

    this.form = this.fb.group({
      username: ['', Validators.required],
      idNumber: [''],
      role: ['', Validators.required],

      emailsInput: [''],
      phonesInput: [''],

      departmentsAndPositions: this.fb.array([])
    });

    this.roleService.list().subscribe(r => (this.roles = r));
    this.branchService.getAll().subscribe(b => (this.branches = b));
    this.departmentService.getAll().subscribe(d => (this.departments = d));

    this.userService.get(username).subscribe(user => this.patchUser(user));
  }

  /* ============================================================
     ASSIGNMENTS (FORM ARRAY)
  ============================================================ */

  get assignments(): FormArray<FormGroup> {
    return this.form.get('departmentsAndPositions') as FormArray<FormGroup>;
  }

  addAssignment(data?: DepartmentAssignmentDTO) {
    const group = this.fb.group({
      branchId: [data?.branchId || '', Validators.required],
      departmentId: [data?.departmentId || '', Validators.required],
      position: [data?.position || 'member', Validators.required]
    });

    // reset department when branch changes
    group.get('branchId')!.valueChanges.subscribe(() => {
      group.get('departmentId')!.reset();
    });

    this.assignments.push(group);
  }

  removeAssignment(index: number) {
    this.assignments.removeAt(index);
  }

  /**
   * Correct filtering:
   * DepartmentDTO does NOT have branchId
   * It has branches: BranchMinimalDTO[]
   */
  departmentsForBranch(branchId: string): DepartmentDTO[] {
    if (!branchId) return [];
    return this.departments.filter(dep =>
      dep.branches?.some(b => b.id === branchId)
    );
  }

  /* ============================================================
     PATCH USER (READ SIDE)
  ============================================================ */

  private patchUser(user: User) {
    this.form.patchValue({
      username: user.username,
      idNumber: user.idNumber,
      role: user.role,
      emailsInput: (user.emailAddresses || []).join(', '),
      phonesInput: (user.phoneNumbers || []).join(', ')
    });

    // Clear existing assignments
    this.assignments.clear();

    // 🔑 READ from branchHierarchy (authoritative read model)
    if (user.branchHierarchy?.length) {
      this.hydrateAssignmentsFromHierarchy(user.branchHierarchy);
    }
  }

  /**
   * Converts READ-ONLY BranchHierarchyDTO[]
   * → WRITE DepartmentAssignmentDTO[]
   */
  private hydrateAssignmentsFromHierarchy(
    hierarchy: BranchHierarchyDTO[]
  ) {
    hierarchy.forEach(branch => {
      branch.departments.forEach(dep => {
        this.addAssignment({
          branchId: branch.branchId,
          departmentId: dep.departmentId,
          position:
            dep.position?.toLowerCase() === 'head'
              ? 'head'
              : 'member'
        });
      });
    });
  }

  /* ============================================================
     SUBMIT (WRITE SIDE)
  ============================================================ */

  submit() {
    if (this.form.invalid) return;

    const payload: Partial<User> = {
      username: this.form.value.username,
      idNumber: this.form.value.idNumber,
      role: this.form.value.role,
      emailAddresses: this.split(this.form.value.emailsInput),
      phoneNumbers: this.split(this.form.value.phonesInput),
      departmentsAndPositions: this.assignments.value
    };

    this.userService.update(this.originalUsername, payload).subscribe({
      next: () => {
        this.snackbar.open('User updated successfully', 'Close', {
          duration: 2000
        });
        this.router.navigate(['/users', payload.username]);
      },
      error: () =>
        this.snackbar.open('Failed to update user', 'Close', {
          duration: 3000
        })
    });
  }

  cancel() {
    this.router.navigate(['/users', this.originalUsername]);
  }

  /* ============================================================
     UTIL
  ============================================================ */

  private split(v: string): string[] {
    return (v || '')
      .split(',')
      .map(x => x.trim())
      .filter(Boolean);
  }
}