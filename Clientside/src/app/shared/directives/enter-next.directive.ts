import {
  Directive,
  ElementRef,
  OnDestroy,
  OnInit
} from '@angular/core';

@Directive({
  selector: '[enterNext]',
  standalone: true
})
export class EnterNextDirective
  implements OnInit, OnDestroy {

  private handler!: (
    e: KeyboardEvent
  ) => void;

  constructor(
    private el: ElementRef<HTMLFormElement>
  ) { }

  ngOnInit(): void {

    const form =
      this.el.nativeElement;

    this.handler =
      (e: KeyboardEvent) => {

        if (e.key !== 'Enter') {
          return;
        }

        const target =
          e.target as HTMLElement;

        const formEl =
          this.el.nativeElement;

        const controls =
          Array.from(
            formEl.querySelectorAll<HTMLElement>(
              `
              input:not([disabled]),
              textarea:not([disabled]),
              mat-select,
              .mat-mdc-select
              `
            )
          );

        const currentIndex =
          controls.findIndex(
            el =>
              el === target ||
              el.contains(target)
          );

        if (currentIndex === -1) {
          return;
        }

        e.preventDefault();

        const nextEmpty =
          controls
            .slice(currentIndex + 1)
            .find(control =>
              this.isEmpty(control)
            );

        if (nextEmpty) {

          this.focusControl(
            nextEmpty
          );

          return;
        }

        formEl.requestSubmit();
      };

    form.addEventListener(
      'keydown',
      this.handler,
      true
    );
  }

  ngOnDestroy(): void {

    this.el.nativeElement
      .removeEventListener(
        'keydown',
        this.handler,
        true
      );
  }

  private isEmpty(
    element: HTMLElement
  ): boolean {

    if (
      element instanceof HTMLInputElement ||
      element instanceof HTMLTextAreaElement
    ) {
      return !element.value?.trim();
    }

    const matSelect =
      element.closest(
        'mat-form-field'
      );

    if (matSelect) {

      const valueText =
        matSelect.querySelector(
          '.mat-mdc-select-value-text'
        )?.textContent?.trim();

      return !valueText;
    }

    return false;
  }

  private focusControl(
    element: HTMLElement
  ): void {

    if (
      element instanceof HTMLInputElement ||
      element instanceof HTMLTextAreaElement
    ) {
      setTimeout(
        () => element.focus(),
        0
      );
      return;
    }

    const selectTrigger =
      element.matches('.mat-mdc-select')
        ? element
        : element.querySelector(
          '.mat-mdc-select'
        );

    if (selectTrigger) {

      setTimeout(() => {
        selectTrigger.dispatchEvent(
          new MouseEvent(
            'mousedown',
            { bubbles: true }
          )
        );
      }, 0);
    }
  }
}