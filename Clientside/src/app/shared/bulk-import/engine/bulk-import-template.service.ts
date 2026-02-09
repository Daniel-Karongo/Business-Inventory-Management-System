import { Injectable } from '@angular/core';
import { saveAs } from 'file-saver';
import * as ExcelJS from 'exceljs';
import { BulkImportConfig } from '../models/bulk-import-config.model';

@Injectable({ providedIn: 'root' })
export class BulkImportTemplateService {

  /* ================= CSV ================= */

  downloadCsv(config: BulkImportConfig<any, any, any>): void {
    const header = config.headers.join(',');
    const example = config.headers
      .map(h => (config.exampleRow as any)?.[h] ?? '')
      .join(',');

    const csv = `${header}\n${example}`;

    saveAs(
      new Blob([csv], { type: 'text/csv' }),
      config.csvFileName
    );
  }

  /* ================= EXCEL ================= */

  async downloadExcel(
    config: BulkImportConfig<any, any, any>
  ): Promise<void> {

    if (!config.excelColumns?.length) {
      throw new Error('excelColumns must be defined');
    }

    const workbook = new ExcelJS.Workbook();
    const worksheet = workbook.addWorksheet(
      config.excelSheetName || 'Sheet',
      { views: [{ state: 'frozen', ySplit: 1 }] }
    );

    /* ===== Columns ===== */
    worksheet.columns = config.excelColumns.map(col => ({
      header: col.header,
      key: col.key,
      width: col.width ?? 18
    }));

    /* ===== Shared Border (INTENTIONALLY UN-TYPED) ===== */
    const border = {
      top: { style: 'thin' },
      left: { style: 'thin' },
      bottom: { style: 'thin' },
      right: { style: 'thin' }
    };

    /* ===== Header ===== */
    const headerRow = worksheet.getRow(1);
    headerRow.font = { bold: true };

    this.styleFullRow(headerRow, border);

    /* ===== Example Row ===== */
    if (config.exampleRow) {
      const row = worksheet.addRow(config.exampleRow as any);
      this.styleFullRow(row, border);
      this.applyFormats(row, config);
    }

    /* ===== Empty Rows ===== */
    const emptyCount = config.emptyRowCount ?? 200;

    for (let i = 0; i < emptyCount; i++) {
      const row = worksheet.addRow(config.emptyRow ?? {});
      this.styleFullRow(row, border);
      this.applyFormats(row, config);
    }

    /* ===== Save ===== */
    const buffer = await workbook.xlsx.writeBuffer();

    saveAs(
      new Blob([buffer], {
        type:
          'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
      }),
      config.excelFileName
    );
  }

  /* ================= Helpers ================= */

  private styleFullRow(
    row: ExcelJS.Row,
    border: any   // 🔑 IMPORTANT: let ExcelJS accept it
  ) {
    const columnCount = row.worksheet.columnCount;

    for (let col = 1; col <= columnCount; col++) {
      const cell = row.getCell(col); // forces cell creation
      cell.alignment = { vertical: 'middle', horizontal: 'left' };
      cell.border = border;
    }
  }

  private applyFormats(
    row: ExcelJS.Row,
    config: BulkImportConfig<any, any, any>
  ) {
    config.excelColumns?.forEach((col, index) => {
      const cell = row.getCell(index + 1);

      if (col.format === 'text') {
        cell.numFmt = '@';
      }

      if (col.format === 'date') {
        cell.numFmt = 'dd/mm/yyyy';
      }
    });
  }
}