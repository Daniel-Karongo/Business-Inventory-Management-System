import { Injectable, ElementRef } from '@angular/core';

@Injectable({ providedIn: 'root' })
export class BulkImportScrollService {

  private container?: HTMLElement;
  private bottom?: HTMLElement;

  register(
    container: ElementRef<HTMLElement>,
    bottom: ElementRef<HTMLElement>
  ) {
    this.container = container.nativeElement;
    this.bottom = bottom.nativeElement;
  }

  scrollTop() {
    this.container?.scrollTo({ top: 0, behavior: 'smooth' });
  }

  scrollBottom() {
    this.bottom?.scrollIntoView({ behavior: 'smooth' });
  }

  goToLine(line: number) {
    if (!this.container || line < 1) return;

    const cards = this.container.querySelectorAll('.row-card');
    const target = cards[line - 1] as HTMLElement;
    if (!target) return;

    target.scrollIntoView({ behavior: 'smooth', block: 'start' });
    target.classList.add('highlight-line');

    setTimeout(() => target.classList.remove('highlight-line'), 1500);
  }
}