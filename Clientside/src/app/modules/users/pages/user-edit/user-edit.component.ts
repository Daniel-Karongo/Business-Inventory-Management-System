import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ActivatedRoute, Router } from '@angular/router';
import { UserService } from '../../services/user/user.service';
import { RoleService } from '../../services/role/role.service';
import { FormBuilder, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatSelectModule } from '@angular/material/select';
import { MatInputModule } from '@angular/material/input';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { User } from '../../models/user.model';
import { DepartmentAssignmentDTO } from '../../../departments/models/department.model';

@Component({
  selector: 'app-user-edit',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatSelectModule,
    MatInputModule,
    MatButtonModule,
    MatSnackBarModule,
    MatIconModule
  ],
  templateUrl: './user-edit.component.html',
  styleUrls: ['./user-edit.component.scss']
})
export class UserEditComponent implements OnInit {
  id?: string;
  form!: FormGroup;

  roles: any[] = [];
  departmentsAndPositions: DepartmentAssignmentDTO[] = [];

  constructor(
    private route: ActivatedRoute,
    private userService: UserService,
    private roleService: RoleService,
    private fb: FormBuilder,
    public router: Router,
    private snackbar: MatSnackBar
  ) {}

  ngOnInit() {
    this.id = this.route.snapshot.paramMap.get('id') ?? undefined;

    this.roleService.list().subscribe(r => (this.roles = r));

    this.form = this.fb.group({
      username: ['', Validators.required],
      emailAddresses: [[]],
      phoneNumbers: [[]],
      idNumber: [''],
      role: ['', Validators.required],
      departmentsAndPositions: [[]]
    });

    if (this.id) {
      this.userService.get(this.id).subscribe(u => {
        u.emailAddresses ||= [];
        u.phoneNumbers ||= [];
        u.departmentsAndPositions ||= [];

        this.departmentsAndPositions = [...u.departmentsAndPositions];

        this.form.patchValue({
          username: u.username,
          emailAddresses: u.emailAddresses,
          phoneNumbers: u.phoneNumbers,
          idNumber: u.idNumber,
          role: u.role,
          departmentsAndPositions: this.departmentsAndPositions
        });
      });
    }
  }

  onEmailsChanged(event: any) {
    const raw = (event.target.value || '') as string;
    const list = raw.split(',').map(x => x.trim()).filter(x => !!x);
    this.form.get('emailAddresses')!.setValue(list);
  }

  onPhonesChanged(event: any) {
    const raw = (event.target.value || '') as string;
    const list = raw.split(',').map(x => x.trim()).filter(x => !!x);
    this.form.get('phoneNumbers')!.setValue(list);
  }

  updateDepartment(i: number, field: keyof DepartmentAssignmentDTO, event: any) {
    this.departmentsAndPositions[i][field] = event.target.value;
    this.form.get('departmentsAndPositions')!.setValue([...this.departmentsAndPositions]);
  }

  addDepartmentAssignment() {
    this.departmentsAndPositions.push({
      branchId: '',
      departmentId: '',
      position: 'member'
    });
    this.form.get('departmentsAndPositions')!.setValue([...this.departmentsAndPositions]);
  }

  removeDepartmentAssignment(i: number) {
    this.departmentsAndPositions.splice(i, 1);
    this.form.get('departmentsAndPositions')!.setValue([...this.departmentsAndPositions]);
  }

  submit() {
    if (!this.id) return;
    if (this.form.invalid) return;

    const payload: Partial<User> = {
      username: this.form.value.username,
      emailAddresses: this.form.value.emailAddresses,
      phoneNumbers: this.form.value.phoneNumbers,
      idNumber: this.form.value.idNumber,
      role: this.form.value.role,
      departmentsAndPositions: this.departmentsAndPositions
    };

    this.userService.update(this.id, payload).subscribe({
      next: () => {
        this.snackbar.open('User updated', 'Close', { duration: 2000 });
        this.router.navigate(['/users']);
      },
      error: () =>
        this.snackbar.open('Failed to update', 'Close', { duration: 3000 })
    });
  }
}