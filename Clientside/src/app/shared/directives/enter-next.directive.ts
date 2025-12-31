import { Directive, HostListener } from '@angular/core';

@Directive({
  selector: '[enterNext]',
  standalone: true
})
export class EnterNextDirective {

  @HostListener('keydown.enter', ['$event'])
  onEnter(event: Event) {
    const e = event as KeyboardEvent;
    e.preventDefault();

    const target = e.target as HTMLElement;
    if (!target) return;

    // Limit traversal to the nearest form (important)
    const form = target.closest('form');
    if (!form) return;

    // Collect focusable fields in order
    const focusables = Array.from(
      form.querySelectorAll<HTMLElement>(
        'input:not([disabled]), textarea:not([disabled]), mat-select:not([disabled])'
      )
    );

    const index = focusables.indexOf(target);

    if (index > -1) {
      const next = focusables[index + 1];

      if (next) {
        next.focus();
      }
    }
  }
}