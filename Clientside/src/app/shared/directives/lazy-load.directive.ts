import { Directive, ElementRef, Input, OnInit } from '@angular/core';

@Directive({
    selector: '[lazyLoad]',
    standalone: true
})
export class LazyLoadDirective implements OnInit {

    @Input() lazyLoad!: () => void;

    constructor(private el: ElementRef) { }

    ngOnInit() {
        const observer = new IntersectionObserver(entries => {
            entries.forEach(entry => {
                if (entry.isIntersecting) {
                    this.lazyLoad?.();
                    observer.disconnect();
                }
            });
        });

        observer.observe(this.el.nativeElement);
    }
}