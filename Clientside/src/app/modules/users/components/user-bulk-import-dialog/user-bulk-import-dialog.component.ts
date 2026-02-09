import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReactiveFormsModule } from '@angular/forms';
import { MatDialogRef } from '@angular/material/dialog';

import { UserService } from '../../services/user/user.service';
import { BranchService } from '../../../branches/services/branch.service';
import { DepartmentService } from '../../../departments/services/department.service';

import { BulkImportFormComponent } from
  '../../../../shared/bulk-import/base/bulk-import-form.component';
import { BulkImportSubmitEngineService } from
  '../../../../shared/bulk-import/engine/bulk-import-submit-engine.service';
import { BulkImportShellComponent } from
  '../../../../shared/bulk-import/shell/bulk-import-shell.component';

import { USER_BULK_IMPORT_CONFIG } from './user-bulk-import.config';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';

@Component({
  standalone: true,
  selector: 'app-user-bulk-import-dialog',
  templateUrl: './user-bulk-import-dialog.component.html',
  styleUrls: ['./user-bulk-import-dialog.component.scss'],
  imports: [
    CommonModule,
    ReactiveFormsModule,
    BulkImportShellComponent,

    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatCheckboxModule,
    MatButtonModule,
    MatIconModule
  ]
})
export class UserBulkImportDialogComponent
  extends BulkImportFormComponent<any, any, any>
  implements OnInit {

  config = USER_BULK_IMPORT_CONFIG;

  branches: any[] = [];
  departments: any[] = [];
  hideOrgFields = false;

  constructor(
    private userService: UserService,
    private branchService: BranchService,
    private departmentService: DepartmentService,
    private submitEngine: BulkImportSubmitEngineService,
    private dialogRef: MatDialogRef<UserBulkImportDialogComponent>
  ) {
    super();
  }

  ngOnInit() {
    this.initForm();
    this.loadOrgMeta();
  }

  private loadOrgMeta() {
    Promise.all([
      this.branchService.getAll(false).toPromise(),
      this.departmentService.getAll(false).toPromise()
    ]).then(([b, d]) => {
      this.branches = b || [];
      this.departments = d || [];
      this.hideOrgFields =
        this.branches.length === 1 &&
        this.departments.length === 1;
    });
  }

  submit() {
    if (this.form.invalid) return;

    const payload = {
      items: this.rows.controls.map(r =>
        this.config.mapRowToItem(r.value)
      ),
      options: {
        dryRun: this.form.value.dryRun,
        skipDuplicates: true
      }
    };

    this.submitEngine.execute({
      submitFn: req => this.userService.bulkImport(req),
      payload,
      rows: this.rows.controls,
      dryRun: this.form.value.dryRun,
      title: this.config.title,
      confirmLabel: this.config.confirmLabel,
      columns: this.config.previewColumns,
      onErrorsApplied: res => {
        this.cacheErrors(res);

        // 🔴 APPLY PREVIEW ADAPTER HERE
        if (this.config.mapPreviewRow && Array.isArray(res.data)) {
          res.data = res.data.map(r => this.config.mapPreviewRow!(r));
        }
      },
      onFinalSuccess: () => this.dialogRef.close(true),
      onConfirmRetry: () => {
        this.form.patchValue({ dryRun: false });
        this.submit();
      }
    });
  }

  async importExcel(file: File | undefined) {
    if (!file) return;

    const mode = await this.confirmMerge();
    if (!mode) return;

    super.importExcelFile(file, undefined, mode);
  }

  async importCsv(file: File | undefined) {
    if (!file) return;

    const mode = await this.confirmMerge();
    if (!mode) return;

    super.importCsvFile(file, mode);
  }

  close() {
    this.dialogRef.close();
  }
}