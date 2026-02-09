import { Injectable } from '@angular/core';
import * as XLSX from 'xlsx';

@Injectable({ providedIn: 'root' })
export class BulkImportEngineService {

  private processInChunks<T>(
    rows: T[],
    chunkSize: number,
    onRow: (row: T) => void,
    onProgress?: (percent: number) => void,
    onDone?: (count: number) => void
  ) {
    const total = rows.length;
    let index = 0;

    const next = () => {
      const end = Math.min(index + chunkSize, total);

      for (; index < end; index++) {
        onRow(rows[index]);
      }

      if (onProgress) {
        onProgress(Math.round((index / total) * 100));
      }

      if (index < total) {
        requestAnimationFrame(next);
      } else {
        onDone?.(total);
      }
    };

    next();
  }

  /* ================= CSV ================= */

  importCsv(
    file: File,
    headers: string[],
    onRow: (row: any) => void,
    onDone: (count: number) => void,
    onProgress?: (percent: number) => void
  ) {
    const reader = new FileReader();

    reader.onload = () => {
      const lines = (reader.result as string).split('\n').slice(1);
      const total = lines.length;

      let processed = 0;

      for (const line of lines) {
        if (!line.trim()) continue;

        const values = line.split(',');
        const row: any = {};

        headers.forEach((h, i) => {
          row[h] = values[i]?.trim();
        });

        onRow(row);

        processed++;
        if (onProgress) {
          onProgress(Math.round((processed / total) * 100));
        }
      }

      onDone(processed);
    };

    reader.readAsText(file);
  }

  /* ================= EXCEL (XLSX) ================= */

  importExcel(
    file: File,
    headers: string[],
    adapt: ((row: any) => any) | null,
    onRow: (row: any) => void,
    onDone: (count: number) => void,
    onProgress?: (percent: number) => void
  ) {
    const reader = new FileReader();

    reader.onload = e => {
      const wb = XLSX.read(e.target?.result, { type: 'binary' });
      const sheet = wb.Sheets[wb.SheetNames[0]];
      const rows = XLSX.utils.sheet_to_json<any>(sheet, { defval: '' });

      this.processInChunks(
        rows.map(r => (adapt ? adapt(r) : r)),
        25,
        onRow,
        onProgress,
        onDone
      );
    };

    reader.readAsBinaryString(file);
  }

  parseExcelDate(value: any): Date | null {
    if (!value) return null;
    if (value instanceof Date) return value;

    if (typeof value === 'number') {
      return new Date((value - 25569) * 86400 * 1000);
    }

    const parsed = new Date(value);
    return isNaN(parsed.getTime()) ? null : parsed;
  }
}