import { Component, OnInit, ViewChild, ElementRef } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormBuilder, FormGroup, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { Router } from '@angular/router';
import { UserService } from '../../services/user/user.service';
import { RoleService } from '../../services/role/role.service';

@Component({
  selector: 'app-user-create',
  standalone: true,
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatButtonModule,
    MatSnackBarModule
  ],
  templateUrl: './user-create.component.html',
  styleUrls: ['./user-create.component.scss']
})
export class UserCreateComponent implements OnInit {

  form!: FormGroup;
  roles: any[] = [];
  fileToUpload?: File;

  constructor(
    private fb: FormBuilder,
    private userService: UserService,
    private roleService: RoleService,
    public router: Router,
    private snackbar: MatSnackBar
  ) {}

  ngOnInit() {

    this.roleService.list().subscribe(r => this.roles = r);

    this.form = this.fb.group({
      username: ['', Validators.required],
      idNumber: [''],
      emailAddresses: [[]],
      phoneNumbers: [[]],
      role: ['', Validators.required],
      password: ['', Validators.required],
      confirmPassword: ['', Validators.required]
    });

  }

  onEmailsChanged(event: any) {
    const raw = event.target.value || '';
    const list = raw.split(',').map((x: String) => x.trim()).filter((x: any) => !!x);
    this.form.get('emailAddresses')!.setValue(list);
  }

  onPhonesChanged(event: any) {
    const raw = event.target.value || '';
    const list = raw.split(',').map((x: string) => x.trim()).filter((x: any) => !!x);
    this.form.get('phoneNumbers')!.setValue(list);
  }

  onFileSelected(event: any) {
    this.fileToUpload = event.target.files?.[0];
  }

  submit() {
    if (this.form.invalid) return;

    if (this.form.value.password !== this.form.value.confirmPassword) {
      this.snackbar.open('Passwords do not match', 'Close', { duration: 2000 });
      return;
    }

    const fd = new FormData();

    // Append simple string fields safely
    fd.append('username', this.form.value.username ?? '');
    fd.append('password', this.form.value.password ?? '');
    fd.append('role', this.form.value.role ?? '');
    fd.append('idNumber', this.form.value.idNumber ?? '');

    // Append JSON arrays (emails + phones)
    fd.append('emailAddresses', JSON.stringify(this.form.value.emailAddresses ?? []));
    fd.append('phoneNumbers', JSON.stringify(this.form.value.phoneNumbers ?? []));

    // File upload
    if (this.fileToUpload) {
      fd.append('userFiles', this.fileToUpload, this.fileToUpload.name);
    }

    this.userService.create(fd).subscribe({
      next: () => {
        this.snackbar.open('User created', 'Close', { duration: 2000 });
        this.router.navigate(['/users']);
      },
      error: (err) => {
        console.error(err);
        this.snackbar.open('Failed to create user', 'Close', { duration: 3000 });
      }
    });
  }
}