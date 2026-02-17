import { Injectable } from '@angular/core';
import JSZip from 'jszip';
import { BulkFileUtilsService } from './bulk-file-utils.service';

export interface BulkZipFileMatch {
  file: File;
  matchedRowIndex?: number;
}

export interface BulkZipMatchResult {
  entries: BulkZipFileMatch[];
}

@Injectable({ providedIn: 'root' })
export class BulkFileImportEngine {

  constructor(private utils: BulkFileUtilsService) { }

  async parseZipAndMatch(
    zipFile: File,
    rows: { name: string; variants?: string[] }[]
  ): Promise<BulkZipMatchResult> {

    const zip = await JSZip.loadAsync(zipFile);

    const entries: BulkZipFileMatch[] = [];

    const files = Object.values(zip.files)
      .filter(f => !f.dir);

    for (const entry of files) {

      const blob = await entry.async('blob');

      const extractedFile = new File(
        [blob],
        entry.name,
        { type: blob.type || this.utils.detectMimeFromName(entry.name) }
      );

      const normalizedFile =
        this.utils.normalizeName(entry.name);

      let bestIndex: number | null = null;
      let bestScore = 0;

      rows.forEach((row, index) => {

        const normalizedRow =
          this.utils.normalizeName(row.name);

        if (!normalizedRow) return;

        // 1️⃣ Direct containment match (strongest)
        if (normalizedFile.includes(normalizedRow)) {
          bestIndex = index;
          bestScore = 1;
          return;
        }

        // 2️⃣ Similarity ratio
        const distance =
          this.levenshtein(normalizedFile, normalizedRow);

        const maxLen =
          Math.max(normalizedFile.length, normalizedRow.length);

        const similarity =
          1 - distance / maxLen;

        if (similarity > bestScore) {
          bestScore = similarity;
          bestIndex = index;
        }
      });

      // Only auto-assign if ≥ 80% similarity
      if (bestScore < 0.8) {
        bestIndex = null;
      }

      entries.push({
        file: extractedFile,
        matchedRowIndex:
          bestIndex !== null ? bestIndex : undefined
      });
    }

    return { entries };
  }

  private levenshtein(a: string, b: string): number {

    const matrix: number[][] = [];

    for (let i = 0; i <= b.length; i++) {
      matrix[i] = [i];
    }

    for (let j = 0; j <= a.length; j++) {
      matrix[0][j] = j;
    }

    for (let i = 1; i <= b.length; i++) {
      for (let j = 1; j <= a.length; j++) {

        if (b.charAt(i - 1) === a.charAt(j - 1)) {
          matrix[i][j] = matrix[i - 1][j - 1];
        } else {
          matrix[i][j] = Math.min(
            matrix[i - 1][j - 1] + 1,
            matrix[i][j - 1] + 1,
            matrix[i - 1][j] + 1
          );
        }
      }
    }

    return matrix[b.length][a.length];
  }
}