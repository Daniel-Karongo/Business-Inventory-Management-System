import {
  Injectable,
  ElementRef
} from '@angular/core';

@Injectable({
  providedIn: 'root'
})
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
    this.container?.scrollTo({
      top: 0,
      behavior: 'smooth'
    });
  }

  scrollBottom() {
    this.bottom?.scrollIntoView({
      behavior: 'smooth'
    });
  }

  getCurrentLine(): number {

    if (!this.container) {
      return 1;
    }

    const cards = Array.from(
      this.container.querySelectorAll<HTMLElement>(
        '.row-card'
      )
    );

    const containerTop =
      this.container.getBoundingClientRect().top;

    for (let i = 0; i < cards.length; i++) {

      const rect =
        cards[i].getBoundingClientRect();

      if (rect.bottom > containerTop + 10) {
        return Number(
          cards[i].dataset['line']
        ) || i + 1;
      }
    }

    return 1;
  }

  goToLine(line: number) {

    if (!this.container || line < 1) {
      return;
    }

    const maxAttempts = 20;

    let attempts = 0;

    const tryFocus = () => {

      const target =
        this.container?.querySelector<HTMLElement>(
          `.row-card[data-line="${line}"]`
        );

      if (target) {

        target.scrollIntoView({
          behavior: 'smooth',
          block: 'center'
        });

        target.classList.remove(
          'highlight-line'
        );

        void target.offsetWidth;

        target.classList.add(
          'highlight-line'
        );

        setTimeout(() => {

          target.classList.remove(
            'highlight-line'
          );

        }, 2000);

        target.focus?.();

        return;
      }

      attempts++;

      /*
        force viewport movement so
        virtual scroll renders row
      */
      this.container!.scrollTop =
        (line - 1) * 72;

      if (attempts < maxAttempts) {

        requestAnimationFrame(
          tryFocus
        );
      }
    };

    tryFocus();
  }
}