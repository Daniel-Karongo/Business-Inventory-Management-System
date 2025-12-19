import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import {
  FormBuilder,
  Validators,
  ReactiveFormsModule,
  FormControl,
  FormGroup
} from '@angular/forms';
import { Router } from '@angular/router';
import { Observable } from 'rxjs';
import { map, startWith } from 'rxjs/operators';

import { MatButtonModule } from '@angular/material/button';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { MatChipsModule } from '@angular/material/chips';
import { MatExpansionModule } from '@angular/material/expansion';

import { BranchService } from '../../../branches/services/branch.service';
import { DepartmentService } from '../../../departments/services/department.service';
import { UserService } from '../../../users/services/user/user.service';

import { DepartmentDTO } from '../../../departments/models/department.model';
import { MinimalUserDTO } from '../../../users/models/user.model';
import { filterBy } from '../../../../core/utils/search-filter';
import { SearchableAssignComponent } from '../../../../shared/components/searchable-assign/searchable-assign.component';

@Component({
  standalone: true,
  selector: 'app-branch-create',
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatButtonModule,
    MatFormFieldModule,
    MatInputModule,
    MatAutocompleteModule,
    MatChipsModule,
    MatExpansionModule,
    SearchableAssignComponent
  ],
  templateUrl: './branch-create.component.html',
  styleUrls: ['./branch-create.component.scss']
})
export class BranchCreateComponent implements OnInit {

  fg!: FormGroup;

  users: MinimalUserDTO[] = [];
  departments: DepartmentDTO[] = [];

  userIds: string[] = [];
  departmentIds: string[] = [];

  userCtrl = new FormControl<string>('');
  deptCtrl = new FormControl<string>('');

  filteredUsers$!: Observable<MinimalUserDTO[]>;
  filteredDepts$!: Observable<DepartmentDTO[]>;

  // ---- display & id helpers (REQUIRED by SearchableAssignComponent) ----
  userDisplay = (u: MinimalUserDTO) => u.username;
  userId = (u: MinimalUserDTO) => u.id;

  deptDisplay = (d: DepartmentDTO) => d.name;
  deptId = (d: DepartmentDTO) => d.id!;

  constructor(
    private fb: FormBuilder,
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

    this.userService.list(0, 500).subscribe(r => {
      this.users = r.data;
      this.filteredUsers$ = this.userCtrl.valueChanges.pipe(
        startWith(''),
        map(v => filterBy(this.users, v ?? '', u => u.username))
      );
    });

    this.deptService.getAll(false).subscribe(d => {
      this.departments = d;
      this.filteredDepts$ = this.deptCtrl.valueChanges.pipe(
        startWith(''),
        map(v => filterBy(this.departments, v ?? '', x => x.name))
      );
    });
  }

  removeUser(id: string) {
    this.userIds = this.userIds.filter(x => x !== id);
  }

  removeDept(id: string) {
    this.departmentIds = this.departmentIds.filter(x => x !== id);
  }

  usernameById(id: string) {
    return this.users.find(u => u.id === id)?.username ?? '';
  }

  deptNameById(id: string) {
    return this.departments.find(d => d.id === id)?.name ?? '';
  }

  save() {
    if (this.fg.invalid) return;

    this.branchService.create({
      ...this.fg.value,
      userIds: this.userIds,
      departmentIds: this.departmentIds
    }).subscribe(() => this.router.navigate(['/branches']));
  }
}