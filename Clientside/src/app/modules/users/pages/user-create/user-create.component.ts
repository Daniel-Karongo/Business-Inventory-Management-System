import {
  Component, OnInit, ChangeDetectorRef, Inject
} from '@angular/core';
import {
  FormArray, FormBuilder, FormGroup, Validators, ReactiveFormsModule
} from '@angular/forms';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';

import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { MatDialog, MatDialogModule, MAT_DIALOG_DATA } from '@angular/material/dialog';

import { FormsModule } from '@angular/forms';
import { finalize, of } from 'rxjs';
import { catchError } from 'rxjs/operators';

import { UserService } from '../../services/user/user.service';
import { BranchService } from '../../../branches/services/branch.service';
import { DepartmentService } from '../../../departments/services/department.service';
import { RoleService } from '../../services/role/role.service';
import { AuthService } from '../../../auth/services/auth.service';

@Component({
  selector: 'app-user-create',
  standalone: true,
  templateUrl: './user-create.component.html',
  styleUrls: ['./user-create.component.scss'],
  imports: [
    CommonModule,
    FormsModule,
    ReactiveFormsModule,
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatButtonModule,
    MatIconModule,
    MatSnackBarModule,
    MatDialogModule
  ]
})
export class UserCreateComponent implements OnInit {

  steps = ['Basic', 'Contacts', 'Role', 'Branches and Departments Assignments', 'Files', 'Review'];
  step = 0;
  maxStep = 5;

  form!: FormGroup;

  branches: any[] = [];
  departments: any[] = [];
  assignableRoles: string[] = [];

  hidePw = true;
  hideConfirmPw = true;
  pwMismatch = false;

  files: File[] = [];
  previews: { src?: string; name: string; type: string }[] = [];
  descriptionOptions = ['ID', 'Passport', 'Signature', 'Utility Bill', 'Other'];
  fileDescriptions: string[] = [];
  customDescriptions: string[] = [];

  busy = false;

  get assignments(): FormArray<FormGroup> {
    return this.form.get('departmentsAndPositions') as FormArray<FormGroup>;
  }

  constructor(
    private fb: FormBuilder,
    private userService: UserService,
    private branchService: BranchService,
    private departmentService: DepartmentService,
    private roleService: RoleService,
    private auth: AuthService,
    private snackbar: MatSnackBar,
    private cdr: ChangeDetectorRef,
    private dialog: MatDialog,
    public router: Router
  ) { }

  ngOnInit() {
    this.form = this.fb.group({
      username: [''],
      idNumber: [''],
      emailAddresses: [[]],
      phoneNumbers: [[]],
      role: ['', Validators.required],
      password: ['', [Validators.required, Validators.minLength(8)]],
      confirmPassword: ['', Validators.required],
      departmentsAndPositions: this.fb.array([])
    }, {
      validators: this.passwordValidator
    });

    this.form.statusChanges.subscribe(() => {
      this.pwMismatch = this.form.hasError('pwMismatch');
    });

    this.loadBranches();
    this.loadDepts();
    this.loadRoles();

    this.addAssignment();
  }

  /* ------------------ Validators ------------------ */
  passwordValidator = (group: FormGroup) => {
    const pw = group.get('password')?.value;
    const cp = group.get('confirmPassword')?.value;
    return pw === cp ? null : { pwMismatch: true };
  };

  /* ------------------ Data Loading ------------------ */
  loadBranches() {
    this.branchService.getAll(false)
      .pipe(catchError(() => of([])))
      .subscribe(res => { this.branches = res; });
  }

  loadDepts() {
    this.departmentService.getAll(false)
      .pipe(catchError(() => of([])))
      .subscribe(res => { this.departments = res; });
  }

  loadRoles() {
    this.roleService.list()
      .pipe(catchError(() => of([])))
      .subscribe((roles: any[]) => {
        const all = roles.map(r => typeof r === 'string' ? r : r.name);
        const order = ['EMPLOYEE', 'SUPERVISOR', 'MANAGER', 'ADMIN', 'SUPERUSER'];
        const myRole = this.auth.getRoles()?.[0];
        const idx = order.indexOf(myRole);
        this.assignableRoles = all.filter(r => order.indexOf(r) <= idx);
      });
  }

  /* ------------------ Contacts Parsing ------------------ */
  onEmailsChange(ev: Event) {
    const value = (ev.target as HTMLInputElement).value;
    const list = value.split(',').map(x => x.trim()).filter(x => x.includes('@'));
    this.form.get('emailAddresses')!.setValue(list);
  }

  onPhonesChange(ev: Event) {
    const value = (ev.target as HTMLInputElement).value;
    const list = value.split(',').map(x => x.trim()).filter(x => x.length >= 7);
    this.form.get('phoneNumbers')!.setValue(list);
  }

  /* ------------------ Assignments ------------------ */
  newAssignment() {
    return this.fb.group({
      branchId: ['', Validators.required],
      departmentId: ['', Validators.required],
      position: ['member', Validators.required]
    });
  }

  addAssignment() { this.assignments.push(this.newAssignment()); }
  removeAssignment(i: number) { this.assignments.removeAt(i); }

  departmentsForBranch(branchId: string) {
    return this.departments.filter(d => d.branchId === branchId);
  }

  getBranchName(id: string) {
    return this.branches.find(b => b.id === id)?.name;
  }

  getDeptName(id: string) {
    return this.departments.find(d => d.id === id)?.name;
  }

  /* ------------------ Files ------------------ */
  openFilePicker() {
    document.getElementById('fileInput')!.click();
  }

  onFilesPicked(ev: any) {
    const list: FileList = ev.target.files;
    if (!list) return;

    for (let i = 0; i < list.length; i++) {
      const file = list.item(i)!;
      this.files.push(file);

      this.fileDescriptions.push('ID');
      this.customDescriptions.push('');

      if (file.type.startsWith('image/')) {
        const reader = new FileReader();
        reader.onload = () => this.previews.push({
          src: reader.result as string, name: file.name, type: file.type
        });
        reader.readAsDataURL(file);
      } else {
        this.previews.push({ name: file.name, type: file.type });
      }
    }
    ev.target.value = '';
  }

  removeFile(i: number) {
    this.files.splice(i, 1);
    this.previews.splice(i, 1);
    this.fileDescriptions.splice(i, 1);
    this.customDescriptions.splice(i, 1);
  }

  viewFile(i: number) {
    this.dialog.open(FileViewerDialog, {
      data: { file: this.files[i], preview: this.previews[i] },
      width: '80%',
      maxWidth: '1100px'
    });
  }

  /* ------------------ Navigation ------------------ */
  prev() { this.step--; }
  next() {
    if (this.step === 2 && this.pwMismatch) return;
    this.step++;
  }
  goTo(i: number) { this.step = i; }

  /* ------------------ Submit ------------------ */
  submit() {
    const fd = new FormData();
    fd.append('username', this.form.value.username);
    fd.append('password', this.form.value.password);
    fd.append('role', this.form.value.role);
    fd.append('idNumber', this.form.value.idNumber);
    fd.append('emailAddresses', JSON.stringify(this.form.value.emailAddresses));
    fd.append('phoneNumbers', JSON.stringify(this.form.value.phoneNumbers));

    this.assignments.value.forEach((a: any, i: number) => {
      fd.append(`departmentsAndPositions[${i}].branchId`, a.branchId);
      fd.append(`departmentsAndPositions[${i}].departmentId`, a.departmentId);
      fd.append(`departmentsAndPositions[${i}].position`, a.position);
    });

    this.files.forEach((file, i) => {
      fd.append(`userFiles[${i}].file`, file, file.name);
      const desc = this.fileDescriptions[i] === 'Other'
        ? this.customDescriptions[i]
        : this.fileDescriptions[i];
      fd.append(`userFiles[${i}].description`, desc);
    });

    this.busy = true;

    this.userService.create(fd)
      .pipe(finalize(() => this.busy = false))
      .subscribe({
        next: () => {
          this.snackbar.open('User created', 'Close', { duration: 2000 });
          this.router.navigate(['/users']);
        },
        error: () =>
          this.snackbar.open('Failed to create user', 'Close', { duration: 2000 })
      });
  }
}

/* ============================================================
   FILE VIEWER DIALOG
============================================================ */
@Component({
  standalone: true,
  imports: [CommonModule, MatDialogModule, MatIconModule, MatButtonModule],
  template: `
    <div class="file-viewer">
      <header class="header">
        <h3>{{ data.preview?.name }}</h3>
        <button mat-icon-button (click)="close()"><mat-icon>close</mat-icon></button>
      </header>

      <div *ngIf="isImage(); else pdfBlock" class="content">
        <img [src]="data.preview?.src" />
      </div>

      <ng-template #pdfBlock>
        <div class="pdf-content">
          <button mat-stroked-button (click)="openPdf()">Open PDF</button>
        </div>
      </ng-template>
    </div>
  `,
  styles: [`
    .header { display:flex; justify-content:space-between; align-items:center; }
    img { width:100%; max-height:80vh; object-fit:contain; background:#000; }
    .pdf-content { padding:20px; text-align:center; }
  `]
})
export class FileViewerDialog {
  constructor(
    @Inject(MAT_DIALOG_DATA) public data: any,
    private dialog: MatDialog
  ) { }

  isImage() {
    return this.data.preview?.src;
  }

  openPdf() {
    const url = URL.createObjectURL(this.data.file);
    window.open(url, '_blank');
  }

  close() { this.dialog.closeAll(); }
}