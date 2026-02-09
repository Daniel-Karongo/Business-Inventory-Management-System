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

  getCurrentLine(): number {
    if (!this.container) return 1;

    const cards = Array.from(
      this.container.querySelectorAll<HTMLElement>('.row-card')
    );

    const containerTop = this.container.getBoundingClientRect().top;

    for (let i = 0; i < cards.length; i++) {
      const rect = cards[i].getBoundingClientRect();
      if (rect.bottom > containerTop + 10) {
        return i + 1;
      }
    }

    return 1;
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