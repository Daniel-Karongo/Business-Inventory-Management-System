import { Injectable } from '@angular/core';
import { BulkFileType } from './bulk-file.model';

@Injectable({ providedIn: 'root' })
export class BulkFileUtilsService {

  detectFileType(file: File): BulkFileType {

    if (file.type.startsWith('image/')) {
      return 'image';
    }

    if (file.type === 'application/pdf') {
      return 'pdf';
    }

    return 'other';
  }

  detectMimeFromName(name: string): string {

    const ext = name.split('.').pop()?.toLowerCase();

    if (!ext) return '';

    if (['png', 'jpg', 'jpeg', 'webp', 'gif'].includes(ext)) {
      return `image/${ext === 'jpg' ? 'jpeg' : ext}`;
    }

    if (ext === 'pdf') {
      return 'application/pdf';
    }

    return '';
  }

  createPreview(file: File): string | undefined {

    if (file.type.startsWith('image/')) {
      return URL.createObjectURL(file);
    }

    return undefined;
  }

  revokePreview(url?: string) {
    if (url) {
      URL.revokeObjectURL(url);
    }
  }

  normalizeName(value: string): string {
    return value
      ?.toLowerCase()
      .replace(/\.[^/.]+$/, '')
      .replace(/[\s\-_]/g, '')
      .trim();
  }

  isImage(file: File): boolean {
    return file.type.startsWith('image/');
  }
}