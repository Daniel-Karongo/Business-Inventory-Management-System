import {
  Component,
  OnInit,
  ViewChild,
  ElementRef,
  AfterViewInit,
  OnDestroy
} from '@angular/core';
import { CommonModule } from '@angular/common';
import {
  FormArray,
  FormBuilder,
  FormGroup,
  Validators,
  ReactiveFormsModule
} from '@angular/forms';

import * as ExcelJS from 'exceljs';
import * as XLSX from 'xlsx';
import { saveAs } from 'file-saver';

import { forkJoin } from 'rxjs';

import { MatDialog, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatSelectModule } from '@angular/material/select';
import { MatTooltipModule } from '@angular/material/tooltip';

import { UserService } from '../../services/user/user.service';
import { BranchService } from '../../../branches/services/branch.service';
import { DepartmentService } from '../../../departments/services/department.service';

import { BulkRequest, BulkResult } from '../../../../shared/models/bulk-import.model';
import { BulkImportResultDialogComponent } from '../../../../shared/components/bulk-import-result-dialog/bulk-import-result-dialog.component';
import { User } from '../../models/user.model';

@Component({
  standalone: true,
  selector: 'app-user-bulk-import-dialog',
  templateUrl: './user-bulk-import-dialog.component.html',
  styleUrls: ['./user-bulk-import-dialog.component.scss'],
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatDialogModule,
    MatButtonModule,
    MatInputModule,
    MatIconModule,
    MatCheckboxModule,
    MatFormFieldModule,
    MatSelectModule,
    MatSnackBarModule,
    MatTooltipModule
  ]
})
export class UserBulkImportDialogComponent
  implements OnInit, AfterViewInit, OnDestroy {

  form!: FormGroup;
  submitting = false;

  branches: any[] = [];
  departments: any[] = [];
  hideOrgFields = false;

  /* 🔴 NEW (ADDITIVE) */
  private errorIndex = 0;
  private errorRows: number[] = [];

  @ViewChild('scrollContainer') scrollContainer!: ElementRef;
  @ViewChild('bottomAnchor') bottomAnchor!: ElementRef;

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<UserBulkImportDialogComponent>,
    private userService: UserService,
    private branchService: BranchService,
    private departmentService: DepartmentService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog
  ) {}

  ngOnInit(): void {
    this.form = this.fb.group({
      dryRun: [true],
      rows: this.fb.array([])
    });

    this.loadOrgMeta();
    this.addRow();
  }

  /* 🔴 NEW (ADDITIVE) */
  ngAfterViewInit() {
    window.addEventListener('keydown', this.handleKeyNav);
  }

  ngOnDestroy() {
    window.removeEventListener('keydown', this.handleKeyNav);
  }

  /* 🔴 NEW (ADDITIVE) */
  handleKeyNav = (e: KeyboardEvent) => {
    if (e.ctrlKey && e.key.toLowerCase() === 'g') {
      e.preventDefault();
      document.querySelector<HTMLInputElement>('#goToLineInput')?.focus();
    }

    if (e.key === 'Enter') {
      const el = document.activeElement as HTMLInputElement;
      if (el?.id === 'goToLineInput') {
        this.goToLine(el.valueAsNumber);
      }
    }
  };

  /* =========================
     FORM HELPERS (UNCHANGED)
     ========================= */

  get rows(): FormArray {
    return this.form.get('rows') as FormArray;
  }

  /* 🔴 NEW (DERIVED ONLY) */
  get rowCount(): number {
    return this.rows.length;
  }

  private createRow(data?: any): FormGroup {
    return this.fb.group({
      username: [data?.username ?? '', Validators.required],
      password: [data?.password ?? '1234'],
      role: [data?.role ?? 'EMPLOYEE'],
      emails: [data?.emails ?? ''],
      phones: [data?.phones ?? ''],
      branchCode: [data?.branchCode ?? ''],
      departmentName: [data?.departmentName ?? ''],
      position: [data?.position ?? 'member'],
      _error: ['']
    });
  }

  addRow(data?: any) {
    this.rows.push(this.createRow(data));
  }

  removeRow(index: number) {
    this.rows.removeAt(index);
  }

  /* =========================
     ORG META (UNCHANGED)
     ========================= */

  private loadOrgMeta() {
    forkJoin([
      this.branchService.getAll(false),
      this.departmentService.getAll(false)
    ]).subscribe(([b, d]) => {
      this.branches = b || [];
      this.departments = d || [];
      this.hideOrgFields =
        this.branches.length === 1 &&
        this.departments.length === 1;
    });
  }

  /* =========================
     CSV (UNCHANGED)
     ========================= */

  downloadCsvTemplate() {
    const csv =
`username,password,role,emails,phones,branchCode,department,position
jdoe,1234,EMPLOYEE,jdoe@company.com,0712345678,MAIN,GENERAL,member`;

    saveAs(new Blob([csv], { type: 'text/csv' }), 'users-bulk-template.csv');
  }

  importCsv(event: Event) {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;

    const reader = new FileReader();
    reader.onload = () => this.parseCsv(reader.result as string);
    reader.readAsText(input.files[0]);
  }

  private parseCsv(text: string) {
    const lines = text.split('\n').slice(1);
    this.rows.clear();

    for (const line of lines) {
      if (!line.trim()) continue;

      const [
        username, password, role, emails, phones,
        branchCode, departmentName, position
      ] = line.split(',');

      this.addRow({
        username: username?.trim(),
        password: password?.trim() || '1234',
        role: role?.trim() || 'EMPLOYEE',
        emails: emails?.trim(),
        phones: phones?.trim(),
        branchCode: branchCode?.trim(),
        departmentName: departmentName?.trim(),
        position: position?.trim() || 'member'
      });
    }

    /* 🔴 NEW (FEEDBACK ONLY) */
    this.snackbar.open(
      `✅ ${this.rowCount} users loaded from CSV`,
      'Close',
      { duration: 3000 }
    );
  }

  /* =========================
     EXCEL (UNCHANGED)
     ========================= */

  async downloadExcelTemplate() {
    const workbook = new ExcelJS.Workbook();
    const worksheet = workbook.addWorksheet('Users', {
      views: [{ state: 'frozen', ySplit: 1 }]
    });

    worksheet.columns = [
      { header: 'username', key: 'username', width: 18 },
      { header: 'password', key: 'password', width: 12 },
      { header: 'role', key: 'role', width: 14 },
      { header: 'emails', key: 'emails', width: 32 },
      { header: 'phones', key: 'phones', width: 22 },
      { header: 'branchCode', key: 'branchCode', width: 14 },
      { header: 'department', key: 'department', width: 18 },
      { header: 'position', key: 'position', width: 12 }
    ];

    const headerRow = worksheet.getRow(1);
    headerRow.font = { bold: true };

    headerRow.eachCell(cell => {
      cell.alignment = { vertical: 'middle', horizontal: 'left' };
      cell.border = {
        top: { style: 'thin' },
        left: { style: 'thin' },
        bottom: { style: 'thin' },
        right: { style: 'thin' }
      };
    });

    const exampleRow = worksheet.addRow({
      username: 'jdoe',
      password: '1234',
      role: 'EMPLOYEE',
      emails: 'jdoe@company.com',
      phones: '0712345678',
      branchCode: 'MAIN',
      department: 'GENERAL',
      position: 'member'
    });

    exampleRow.eachCell(cell => {
      cell.alignment = { vertical: 'middle', horizontal: 'left' };
      cell.border = {
        top: { style: 'thin' },
        left: { style: 'thin' },
        bottom: { style: 'thin' },
        right: { style: 'thin' }
      };
    });

    exampleRow.getCell('phones').numFmt = '@';

    const ROW_COUNT = 200;

    for (let i = 0; i < ROW_COUNT; i++) {
      const row = worksheet.addRow({
        username: '',
        password: '1234',
        role: 'EMPLOYEE',
        emails: '',
        phones: '',
        branchCode: '',
        department: '',
        position: 'member'
      });

      row.eachCell(cell => {
        cell.alignment = { vertical: 'middle', horizontal: 'left' };
        cell.border = {
          top: { style: 'thin' },
          left: { style: 'thin' },
          bottom: { style: 'thin' },
          right: { style: 'thin' }
        };
      });

      row.getCell('phones').numFmt = '@';
    }

    const buffer = await workbook.xlsx.writeBuffer();

    saveAs(
      new Blob([buffer], {
        type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
      }),
      'users-bulk-template.xlsx'
    );
  }

  importExcel(event: Event) {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;

    const reader = new FileReader();
    reader.onload = e => {
      const wb = XLSX.read(e.target?.result, { type: 'binary' });
      const sheet = wb.Sheets[wb.SheetNames[0]];
      const rows = XLSX.utils.sheet_to_json<any>(sheet, { defval: '' });

      this.rows.clear();

      rows.forEach(r => {
        this.addRow({
          username: r.username,
          password: r.password || '1234',
          role: r.role || 'EMPLOYEE',
          emails: r.emails,
          phones: r.phones,
          branchCode: r.branchCode,
          departmentName: r.department,
          position: r.position || 'member'
        });
      });

      /* 🔴 NEW (FEEDBACK ONLY) */
      this.snackbar.open(
        `✅ ${this.rowCount} users loaded from Excel`,
        'Close',
        { duration: 3000 }
      );
    };

    reader.readAsBinaryString(input.files[0]);
  }

  /* 🔴 NEW (SCROLL + ERRORS) */

  scrollToTop() {
    this.scrollContainer?.nativeElement.scrollTo({ top: 0, behavior: 'smooth' });
  }

  scrollToBottom() {
    this.bottomAnchor?.nativeElement.scrollIntoView({ behavior: 'smooth' });
  }

  goToLine(line: number) {
    if (!line || line < 1 || line > this.rows.length) return;

    const cards = this.scrollContainer.nativeElement.querySelectorAll('.row-card');
    const target = cards[line - 1] as HTMLElement;
    if (!target) return;

    target.scrollIntoView({ behavior: 'smooth', block: 'start' });
    target.classList.add('highlight-line');
    setTimeout(() => target.classList.remove('highlight-line'), 1500);
  }

  private cacheErrors(res: BulkResult<any>) {
    this.errorRows = res.errors?.map(e => e.row) || [];
    this.errorIndex = 0;
  }

  goToNextError() {
    if (!this.errorRows.length) return;
    this.goToLine(this.errorRows[this.errorIndex]);
    this.errorIndex = (this.errorIndex + 1) % this.errorRows.length;
  }

  /* =========================
     SUBMIT (UNCHANGED)
     ========================= */

  submit() {
    if (this.form.invalid) return;

    this.submitting = true;

    const payload: BulkRequest<any> = {
      items: this.rows.controls.map(r => ({
        username: r.value.username,
        password: r.value.password,
        role: r.value.role,
        emailAddresses: [r.value.emails],
        phoneNumbers: [r.value.phones],
        branchCode: this.hideOrgFields ? null : r.value.branchCode,
        departmentName: this.hideOrgFields ? null : r.value.departmentName,
        position: r.value.position
      })),
      options: {
        dryRun: this.form.value.dryRun,
        skipDuplicates: true
      }
    };

    this.userService.bulkImport(payload).subscribe({
      next: (res: BulkResult<User>) => {

        this.rows.controls.forEach(r =>
          r.patchValue({ _error: '' }, { emitEvent: false })
        );

        res.errors?.forEach(e => {
          this.rows.at(e.row - 1)?.patchValue({ _error: e.message });
        });

        /* 🔴 NEW */
        if (res.errors?.length) {
          setTimeout(() => this.goToLine(res.errors[0].row), 200);
        }

        this.cacheErrors(res);

        if (!this.form.value.dryRun && res.failed === 0) {
          this.snackbar.open(
            `${res.success} users imported successfully`,
            'Close',
            { duration: 3000 }
          );
          this.dialogRef.close(true);
          return;
        }

        this.dialog.open(BulkImportResultDialogComponent, {
          width: '1100px',
          maxWidth: '95vw',
          data: {
            title: 'User Import Preview',
            dryRun: this.form.value.dryRun,
            result: res,
            confirmLabel: 'Import Now'
          }
        }).afterClosed().subscribe(confirm => {
          if (confirm) {
            this.form.patchValue({ dryRun: false });
            this.submit();
          }
        });

        this.submitting = false;
      },
      error: () => {
        this.submitting = false;
        this.snackbar.open(
          'User bulk import failed',
          'Close',
          { duration: 3000 }
        );
      }
    });
  }

  close() {
    this.dialogRef.close();
  }
}