import { Injectable } from '@angular/core';
import { DatePipe } from '@angular/common';

@Injectable({ providedIn: 'root' })
export class DateUtilsService {

  constructor(private datePipe: DatePipe) {}

  formatFull(date: string | Date | undefined | null): string {
    if (!date) return '';
    return this.datePipe.transform(
      date,
      'EEEE, d/M/yyyy, h:mm a'
    ) ?? '';
  }

  formatShort(date: string | Date | undefined | null): string {
    if (!date) return '';
    return this.datePipe.transform(
      date,
      'd/M/yyyy'
    ) ?? '';
  }
}