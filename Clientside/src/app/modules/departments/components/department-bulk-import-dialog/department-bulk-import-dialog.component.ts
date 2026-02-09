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

import { MatDialog, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatButtonModule } from '@angular/material/button';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatTooltipModule } from '@angular/material/tooltip';

import { DepartmentService } from '../../services/department.service';
import { BranchService } from '../../../branches/services/branch.service';
import { UserService } from '../../../users/services/user/user.service';
import { BulkRequest, BulkResult } from '../../../../shared/models/bulk-import.model';
import { BulkImportResultDialogComponent } from '../../../../shared/components/bulk-import-result-dialog/bulk-import-result-dialog.component';
import { DepartmentDTO } from '../../models/department.model';

@Component({
  standalone: true,
  selector: 'app-department-bulk-import-dialog',
  templateUrl: './department-bulk-import-dialog.component.html',
  styleUrls: ['./department-bulk-import-dialog.component.scss'],
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatDialogModule,
    MatButtonModule,
    MatInputModule,
    MatIconModule,
    MatCheckboxModule,
    MatFormFieldModule,
    MatSnackBarModule,
    MatTooltipModule
  ]
})
export class DepartmentBulkImportDialogComponent
  implements OnInit, AfterViewInit, OnDestroy {

  form!: FormGroup;
  submitting = false;

  /* 🔴 NEW */
  private errorIndex = 0;
  private errorRows: number[] = [];

  /* 🔴 NEW */
  @ViewChild('scrollContainer') scrollContainer!: ElementRef;
  @ViewChild('bottomAnchor') bottomAnchor!: ElementRef;

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<DepartmentBulkImportDialogComponent>,
    private departmentService: DepartmentService,
    private branchService: BranchService,
    private userService: UserService,
    private snackbar: MatSnackBar,
    private dialog: MatDialog
  ) {}

  ngOnInit(): void {
    this.form = this.fb.group({
      dryRun: [true],
      updateExisting: [false],
      rows: this.fb.array([])
    });

    this.addRow();
  }

  /* 🔴 NEW */
  ngAfterViewInit() {
    window.addEventListener('keydown', this.handleKeyNav);
  }

  ngOnDestroy() {
    window.removeEventListener('keydown', this.handleKeyNav);
  }

  /* 🔴 NEW */
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

  get rows(): FormArray {
    return this.form.get('rows') as FormArray;
  }

  /* 🔴 NEW (DERIVED ONLY) */
  get rowCount(): number {
    return this.rows.length;
  }

  private createRow(data?: any): FormGroup {
    return this.fb.group({
      name: [data?.name ?? '', Validators.required],
      description: [data?.description ?? ''],
      rollcallStartTime: [data?.rollcallStartTime ?? '09:00'],
      gracePeriodMinutes: [data?.gracePeriodMinutes ?? 15],
      branchCodes: [data?.branchCodes ?? 'MAIN', Validators.required],
      headUsernames: [data?.headUsernames ?? ''],
      memberUsernames: [data?.memberUsernames ?? ''],
      _error: ['']
    });
  }

  addRow(data?: any) {
    this.rows.push(this.createRow(data));
  }

  removeRow(i: number) {
    this.rows.removeAt(i);
  }

  /* =========================
     CSV (UNCHANGED)
     ========================= */

  downloadCsvTemplate() {
    const csv =
`name,description,rollcallStartTime,gracePeriodMinutes,branchCodes,headUsernames,memberUsernames
Management,Cash handling and budgeting,09:00,15,MAIN,Samuel Kimani;Mary Wanjiku,Catherine Wairimu`;

    saveAs(new Blob([csv], { type: 'text/csv' }), 'departments-bulk-template.csv');
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
        name,
        description,
        rollcallStartTime,
        gracePeriodMinutes,
        branchCodes,
        headUsernames,
        memberUsernames
      ] = line.split(',');

      if (!name?.trim()) continue;

      this.addRow({
        name: name?.trim(),
        description: description?.trim(),
        rollcallStartTime: rollcallStartTime?.trim(),
        gracePeriodMinutes: gracePeriodMinutes?.trim(),
        branchCodes: branchCodes?.trim(),
        headUsernames: headUsernames?.trim(),
        memberUsernames: memberUsernames?.trim()
      });
    }

    /* 🔴 NEW */
    this.snackbar.open(
      `✅ ${this.rowCount} departments loaded from CSV`,
      'Close',
      { duration: 3000 }
    );
  }

  /* =========================
     EXCEL (UNCHANGED)
     ========================= */

  async downloadExcelTemplate() {
    const workbook = new ExcelJS.Workbook();
    const worksheet = workbook.addWorksheet('Departments', {
      views: [{ state: 'frozen', ySplit: 1 }]
    });

    worksheet.columns = [
      { header: 'name', key: 'name', width: 26 },
      { header: 'description', key: 'description', width: 36 },
      { header: 'rollcallStartTime', key: 'rollcallStartTime', width: 20 },
      { header: 'gracePeriodMinutes', key: 'gracePeriodMinutes', width: 22 },
      { header: 'branchCodes', key: 'branchCodes', width: 22 },
      { header: 'headUsernames', key: 'headUsernames', width: 28 },
      { header: 'memberUsernames', key: 'memberUsernames', width: 32 }
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
      name: 'Management',
      description: 'Cash handling, budgeting, payroll, and financial records',
      rollcallStartTime: '09:00',
      gracePeriodMinutes: 15,
      branchCodes: 'MAIN',
      headUsernames: 'Samuel Kimani;Mary Wanjiku',
      memberUsernames: 'Catherine Wairimu'
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

    exampleRow.getCell('rollcallStartTime').numFmt = '@';
    exampleRow.getCell('branchCodes').numFmt = '@';
    exampleRow.getCell('headUsernames').numFmt = '@';
    exampleRow.getCell('memberUsernames').numFmt = '@';

    for (let i = 0; i < 200; i++) {
      const row = worksheet.addRow({
        name: '',
        description: '',
        rollcallStartTime: '09:00',
        gracePeriodMinutes: 15,
        branchCodes: 'MAIN',
        headUsernames: '',
        memberUsernames: ''
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

      row.getCell('rollcallStartTime').numFmt = '@';
      row.getCell('branchCodes').numFmt = '@';
      row.getCell('headUsernames').numFmt = '@';
      row.getCell('memberUsernames').numFmt = '@';
    }

    const buffer = await workbook.xlsx.writeBuffer();
    saveAs(
      new Blob([buffer], {
        type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
      }),
      'departments-bulk-template.xlsx'
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

      rows
        .filter(r => String(r.name || '').trim())
        .forEach(r => {
          this.addRow({
            name: String(r.name || '').trim(),
            description: String(r.description || '').trim(),
            rollcallStartTime: String(r.rollcallStartTime || '').trim(),
            gracePeriodMinutes: String(r.gracePeriodMinutes || '').trim(),
            branchCodes: String(r.branchCodes || '').trim(),
            headUsernames: String(r.headUsernames || '').trim(),
            memberUsernames: String(r.memberUsernames || '').trim()
          });
        });

      /* 🔴 NEW */
      this.snackbar.open(
        `✅ ${this.rowCount} departments loaded from Excel`,
        'Close',
        { duration: 3000 }
      );
    };

    reader.readAsBinaryString(input.files[0]);
  }

  /* =========================
     SUBMIT (UNCHANGED)
     ========================= */

  private formatResultForPreview(res: BulkResult<DepartmentDTO>): BulkResult<any> {
    return {
      ...res,
      data: res.data.map(row => ({
        ...row,
        branches: Array.isArray(row.branches)
          ? row.branches.map(b => b.branchCode || b.name).join(', ')
          : '',
        heads: Array.isArray(row.heads)
          ? row.heads.map(h => h.username).join(', ')
          : '',
        members: Array.isArray(row.members)
          ? row.members.map(m => m.username).join(', ')
          : ''
      }))
    };
  }

  submit() {
    if (this.form.invalid) return;
    this.submitting = true;

    const payload: BulkRequest<any> = {
      items: this.rows.controls.map(r => ({
        name: r.value.name,
        description: r.value.description,
        rollcallStartTime: r.value.rollcallStartTime,
        gracePeriodMinutes: r.value.gracePeriodMinutes
          ? Number(r.value.gracePeriodMinutes)
          : null,
        branchCodes: r.value.branchCodes
          ? r.value.branchCodes.split(',').map((v: string) => v.trim())
          : [],
        headUsernames: r.value.headUsernames
          ? r.value.headUsernames.split(',').map((v: string) => v.trim())
          : [],
        memberUsernames: r.value.memberUsernames
          ? r.value.memberUsernames.split(',').map((v: string) => v.trim())
          : []
      })),
      options: {
        dryRun: this.form.value.dryRun,
        updateExisting: this.form.value.updateExisting,
        skipDuplicates: true
      }
    };

    this.departmentService.bulkImport(payload).subscribe({
      next: (res: BulkResult<DepartmentDTO>) => {

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
            `${res.success} departments imported successfully`,
            'Close',
            { duration: 3000 }
          );
          this.dialogRef.close(true);
          return;
        }

        const previewResult = this.formatResultForPreview(res);

        this.dialog.open(BulkImportResultDialogComponent, {
          width: '1100px',
          maxWidth: '95vw',
          data: {
            title: 'Department Import Preview',
            dryRun: this.form.value.dryRun,
            result: previewResult,
            confirmLabel: 'Import Now',
            columns: [
              { key: 'name', label: 'Name' },
              { key: 'branches', label: 'Branches' },
              { key: 'heads', label: 'Heads' },
              { key: 'members', label: 'Members' }
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
          'Department bulk import failed',
          'Close',
          { duration: 3000 }
        );
      }
    });
  }

  /* 🔴 NEW */
  private cacheErrors(res: BulkResult<any>) {
    this.errorRows = res.errors?.map(e => e.row) || [];
    this.errorIndex = 0;
  }

  /* 🔴 NEW */
  goToNextError() {
    if (!this.errorRows.length) return;
    this.goToLine(this.errorRows[this.errorIndex]);
    this.errorIndex = (this.errorIndex + 1) % this.errorRows.length;
  }

  /* 🔴 NEW */
  scrollToTop() {
    this.scrollContainer?.nativeElement.scrollTo({ top: 0, behavior: 'smooth' });
  }

  /* 🔴 NEW */
  scrollToBottom() {
    this.bottomAnchor?.nativeElement.scrollIntoView({ behavior: 'smooth' });
  }

  /* 🔴 NEW */
  goToLine(line: number) {
    if (!line || line < 1 || line > this.rows.length) return;

    const cards = this.scrollContainer.nativeElement.querySelectorAll('.row-card');
    const target = cards[line - 1] as HTMLElement;
    if (!target) return;

    target.scrollIntoView({ behavior: 'smooth', block: 'start' });
    target.classList.add('highlight-line');
    setTimeout(() => target.classList.remove('highlight-line'), 1500);
  }

  close() {
    this.dialogRef.close();
  }
}