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
import { DepartmentDTO } from '../../../departments/models/department.model';
import { BranchDTO } from '../../../branches/models/branch.model';
import { FileViewerDialog } from '../../../../shared/components/file-viewer/file-viewer.component';

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

  branches: BranchDTO[] = [];
  departments: DepartmentDTO[] = [];
  assignableRoles: string[] = [];

  hidePw = true;
  hideConfirmPw = true;

  files: File[] = [];
  previews: { src?: string; name: string; type: string }[] = [];
  descriptionOptions = ['ID', 'Passport', 'Signature', 'CV', 'Other'];

  get filesArray(): FormArray {
    return this.form.get('files') as FormArray;
  }

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
      username: ['', Validators.required],
      idNumber: [''],
      emailAddresses: [[]],
      phoneNumbers: [[]],
      role: ['', Validators.required],
      password: ['', [Validators.required, Validators.minLength(8)]],
      confirmPassword: ['', Validators.required],
      departmentsAndPositions: this.fb.array([]),

      files: this.fb.array([])
    }, {
      validators: this.passwordValidator
    });


    this.loadBranches();
    this.loadDepts();
    this.loadRoles();

    this.addAssignment();
  }

  newFileGroup(file: File) {
    const fg = this.fb.group({
      file: [file],
      type: ['ID', Validators.required],
      custom: ['']
    });

    fg.get('type')!.valueChanges.subscribe(v => {
      const c = fg.get('custom');
      if (v === 'Other') {
        c?.setValidators([Validators.required, Validators.minLength(3)]);
      } else {
        c?.clearValidators();
        c?.setValue('');
      }
      c?.updateValueAndValidity();
    });

    return fg;
  }

  get pwMismatch() {
    return this.form.get('confirmPassword')?.hasError('pwMismatch');
  }

  /* ------------------ Validators ------------------ */
  passwordValidator(form: FormGroup) {
    const pw = form.get('password')!.value;
    const cpw = form.get('confirmPassword')!.value;

    const confirmControl = form.get('confirmPassword');

    if (!pw || !cpw) {
      confirmControl?.setErrors(null);
      return null;
    }

    if (pw !== cpw) {
      confirmControl?.setErrors({ pwMismatch: true });
      return { pwMismatch: true };
    }

    confirmControl?.setErrors(null);
    return null;
  }

  validateEmailsPhones(): boolean {
    const emails: string[] = this.form.get('emailAddresses')!.value || [];
    const phones: string[] = this.form.get('phoneNumbers')!.value || [];

    // Email regex
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;

    for (const e of emails) {
      if (e && !emailRegex.test(e)) {
        this.snackbar.open(`Invalid email: ${e}`, 'Close', { duration: 1800 });
        return false;
      }
    }

    // Phone regex: +country-code OR 07xxxxxxx OR 01xxxxxxx
    const phoneRegex = /^\+?\d{7,15}$/;

    for (const p of phones) {
      if (p && !phoneRegex.test(p)) {
        this.snackbar.open(`Invalid phone number: ${p}`, 'Close', { duration: 1800 });
        return false;
      }
    }

    return true;
  }

  validateAssignments(): boolean {
    const arr = this.assignments.controls;

    // 1️⃣ Required fields
    for (let i = 0; i < arr.length; i++) {
      const row = arr[i].value;

      if (!row.branchId || !row.departmentId) {
        this.snackbar.open("Fill all assignment fields", "Close", { duration: 1800 });
        return false;
      }
    }

    // 2️⃣ Detect duplicates — same branch + department
    const seen = new Set<string>();
    for (let i = 0; i < arr.length; i++) {
      const row = arr[i].value;
      const key = `${row.branchId}::${row.departmentId}`;

      if (seen.has(key)) {
        this.snackbar.open("Duplicate branch/department assignment", "Close", { duration: 1800 });
        return false;
      }
      seen.add(key);
    }

    // 3️⃣ Head/Member conflict detection
    const deptMap = new Map<string, Set<string>>();

    for (let i = 0; i < arr.length; i++) {
      const row = arr[i].value;
      const dept = row.departmentId;
      const pos = (row.position || "").toLowerCase();

      const set = deptMap.get(dept) || new Set<string>();
      set.add(pos);
      deptMap.set(dept, set);
    }

    for (const [dept, set] of deptMap.entries()) {
      if (set.has("head") && set.has("member")) {
        this.snackbar.open(
          "A department cannot have the same user as both Head and Member",
          "Close",
          { duration: 2000 }
        );
        return false;
      }
    }

    return true;
  }

  validateStep(step: number): boolean {
    switch (step) {

      /** STEP 0 — BASIC INFO */
      case 0:
        return this.form.get('username')!.valid;

      /** STEP 1 — CONTACTS (emails / phones) */
      case 1:
        return this.validateEmailsPhones(); // your function

      /** STEP 2 — ROLE + PASSWORD */
      case 2:
        const roleValid = this.form.get('role')!.valid;
        const pwValid = !this.pwMismatch &&
          this.form.get('password')!.valid &&
          this.form.get('confirmPassword')!.valid;
        return roleValid && pwValid;

      /** STEP 3 — ASSIGNMENTS */
      case 3:
        return this.validateAssignments(); // your existing method

      /** STEP 4 — FILES (always valid) */
      case 4:
        return true;

      /** STEP 5 — REVIEW (always valid, final) */
      case 5:
        return true;

      default:
        return false;
    }
  }

  /* ------------------ Data Loading ------------------ */
  loadBranches() {
    this.branchService.getAll(false)
      .pipe(catchError(() => of([])))
      .subscribe(res => {
        console.log(res);
        this.branches = res;
      });
  }

  loadDepts() {
    this.departmentService.getAll(false)
      .pipe(catchError(() => of([])))
      .subscribe(res => {
        console.log(res);
        this.departments = res;
      });
  }

  loadRoles() {
    this.roleService.list()
      .pipe(catchError(() => of([])))
      .subscribe((roles: any[]) => {
        const all = roles.map(r => typeof r === 'string' ? r : r.name);
        const order = ['EMPLOYEE', 'SUPERVISOR', 'MANAGER', 'ADMIN', 'SUPERUSER'];
        const myRole = this.auth.getSnapshot()?.role ?? '';
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
    return this.departments.filter(d =>
      d.branches?.some(b => b.id === branchId)
    );
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
      this.filesArray.push(this.newFileGroup(file));

      if (file.type.startsWith('image/')) {
        const reader = new FileReader();
        reader.onload = () => {
          this.previews.push({
            src: reader.result as string,
            name: file.name,
            type: file.type
          });
        };
        reader.readAsDataURL(file);
      } else {
        this.previews.push({
          name: file.name,
          type: file.type
        });
      }
    }

    ev.target.value = '';
  }

  removeFile(i: number) {
    this.files.splice(i, 1);
    this.previews.splice(i, 1);
    this.filesArray.removeAt(i);
  }

  viewFile(i: number) {
    this.dialog.open(FileViewerDialog, {
      data: { file: this.files[i], preview: this.previews[i] },
      width: '80%',
      maxWidth: '1100px'
    });
  }

  /* ------------------ Navigation ------------------ */
  next() {
    if (!this.validateStep(this.step)) {
      this.snackbar.open("Please complete this step before continuing", "Close", { duration: 1800 });
      return;
    }

    if (this.step < this.maxStep) {
      this.step++;
    }
  }

  prev() {
    if (this.step > 0) {
      this.step--;
    }
  }

  goTo(targetStep: number) {
    // Going backward is always allowed
    if (targetStep < this.step) {
      this.step = targetStep;
      return;
    }

    // Validate all previous steps
    for (let i = 0; i < targetStep; i++) {
      if (!this.validateStep(i)) {
        this.snackbar.open("Please complete previous steps first", "Close", { duration: 1800 });
        return;
      }
    }

    this.step = targetStep;
  }


  /* ------------------ Submit ------------------ */
  submit() {
    const fd = new FormData();
    fd.append('username', this.form.value.username);
    fd.append('password', this.form.value.password);
    fd.append('role', this.form.value.role);
    fd.append('idNumber', this.form.value.idNumber);
    // EMAILS
    this.form.value.emailAddresses?.forEach((email: string, i: number) => {
      fd.append(`emailAddresses[${i}]`, email);
    });

    // PHONES
    this.form.value.phoneNumbers?.forEach((phone: string, i: number) => {
      fd.append(`phoneNumbers[${i}]`, phone);
    });

    this.assignments.value.forEach((a: any, i: number) => {
      fd.append(`departmentsAndPositions[${i}].branchId`, a.branchId);
      fd.append(`departmentsAndPositions[${i}].departmentId`, a.departmentId);
      fd.append(`departmentsAndPositions[${i}].position`, a.position);
    });

    this.filesArray.controls.forEach((ctrl, i) => {
      const file = ctrl.value.file;
      const type = ctrl.value.type;
      const custom = ctrl.value.custom;

      fd.append(`userFiles[${i}].file`, file, file.name);
      fd.append(
        `userFiles[${i}].description`,
        type === 'Other' ? custom : type
      );
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