import { Component, OnInit } from '@angular/core';
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

import { MatDialog, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatSelectModule } from '@angular/material/select';

import { forkJoin } from 'rxjs';

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
    MatSnackBarModule
  ]
})
export class UserBulkImportDialogComponent implements OnInit {

  form!: FormGroup;
  submitting = false;

  branches: any[] = [];
  departments: any[] = [];
  hideOrgFields = false;

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<UserBulkImportDialogComponent>,
    private userService: UserService,
    private branchService: BranchService,
    private departmentService: DepartmentService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog
  ) { }

  ngOnInit(): void {
    this.form = this.fb.group({
      dryRun: [false],
      rows: this.fb.array([])
    });

    this.loadOrgMeta();
    this.addRow();
  }

  /* =========================
     FORM HELPERS
     ========================= */

  get rows(): FormArray {
    return this.form.get('rows') as FormArray;
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
     ORG META
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
     CSV
     ========================= */

  downloadCsvTemplate() {
    const csv =
      `username,password,role,emails,phones,branchCode,department,position
jdoe,1234,EMPLOYEE,jdoe@company.com,0712345678,MAIN,GENERAL,member`;

    const blob = new Blob([csv], { type: 'text/csv' });
    saveAs(blob, 'users-bulk-template.csv');
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
  }

  /* =========================
     EXCEL
     ========================= */

  async downloadExcelTemplate() {

    const workbook = new ExcelJS.Workbook();
    const worksheet = workbook.addWorksheet('Users', {
      views: [{ state: 'frozen', ySplit: 1 }]
    });

    /* ============================
       DEFINE COLUMNS
       ============================ */
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

    /* ============================
       HEADER STYLING
       ============================ */
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

    /* ============================
       EXAMPLE ROW
       ============================ */
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

    // 🔥 FORCE PHONES COLUMN TO TEXT
    exampleRow.getCell('phones').numFmt = '@';

    /* ============================
       EMPTY DATA ROWS
       ============================ */
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

      // 🔥 FORCE PHONES COLUMN TO TEXT (ALL ROWS)
      row.getCell('phones').numFmt = '@';
    }

    /* ============================
       FINALIZE
       ============================ */
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
    };

    reader.readAsBinaryString(input.files[0]);
  }

  /* =========================
     SUBMIT
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
            confirmLabel: 'Import Now',
            columns: [
              { key: 'username', label: 'Username' },
              { key: 'role', label: 'Role' },
              { key: 'emailAddresses', label: 'Emails' },
              { key: 'phoneNumbers', label: 'Phones' },
              { key: 'branchCode', label: 'Branch' },
              { key: 'departmentName', label: 'Department' },
              { key: 'position', label: 'Position' }
            ]
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