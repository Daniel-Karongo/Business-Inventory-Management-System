import { Directive, ElementRef, OnDestroy, OnInit } from '@angular/core';

@Directive({
  selector: '[enterNext]',
  standalone: true
})
export class EnterNextDirective implements OnInit, OnDestroy {

  private handler!: (e: KeyboardEvent) => void;

  constructor(private el: ElementRef<HTMLFormElement>) { }

  ngOnInit() {

    const form = this.el.nativeElement;

    this.handler = (e: KeyboardEvent) => {

      if (e.key !== 'Enter') return;

      const target = e.target as HTMLElement;

      if (!(target instanceof HTMLInputElement || target instanceof HTMLTextAreaElement)) {
        return;
      }

      e.preventDefault();

      const formEl = this.el.nativeElement;

      const focusables = Array.from(
        formEl.querySelectorAll<HTMLInputElement | HTMLTextAreaElement>(
          'input:not([disabled]), textarea:not([disabled])'
        )
      );

      const index = focusables.findIndex(el => el === target);
      if (index === -1) return;

      // 🔥 find next EMPTY field (not just next DOM element)
      const nextEmpty = focusables.slice(index + 1).find(el => !el.value);

      if (nextEmpty) {
        setTimeout(() => nextEmpty.focus(), 0);
        return;
      }

      // 🔥 no empty fields ahead → submit
      formEl.requestSubmit();
    };

    // 🔥 Attach at native level (bypasses Angular + Material interference)
    form.addEventListener('keydown', this.handler, true); // use capture phase
  }

  ngOnDestroy() {
    this.el.nativeElement.removeEventListener('keydown', this.handler, true);
  }
}