import {
    Directive,
    ElementRef,
    EventEmitter,
    Input,
    NgZone,
    OnChanges,
    OnDestroy,
    Output,
    SimpleChanges,
    inject
} from '@angular/core';

@Directive({
    selector: '[appLazyThumbnail]',
    standalone: true
})
export class LazyThumbnailDirective
    implements OnChanges, OnDestroy {

    private readonly element =
        inject(ElementRef<HTMLElement>);

    private readonly zone =
        inject(NgZone);

    private observer?: IntersectionObserver;

    private loaded = false;

    @Input()
    appLazyThumbnail: boolean | '' = true;

    @Output()
    visible =
        new EventEmitter<void>();

    ngOnChanges(
        changes: SimpleChanges
    ): void {

        if (!this.appLazyThumbnail) {
            this.disconnect();
            return;
        }

        if (
            changes['appLazyThumbnail']
            && !this.loaded
        ) {
            this.observe();
        }
    }

    ngOnDestroy(): void {
        this.disconnect();
    }

    private observe(): void {

        if (
            this.loaded ||
            this.observer
        ) {
            return;
        }

        this.zone.runOutsideAngular(() => {

            this.observer =
                new IntersectionObserver(
                    entries => {

                        const entry =
                            entries[0];

                        if (
                            !entry?.isIntersecting
                        ) {
                            return;
                        }

                        this.loaded = true;

                        this.disconnect();

                        this.zone.run(() => {
                            this.visible.emit();
                        });
                    },
                    {
                        root: null,
                        rootMargin: '200px',
                        threshold: 0.01
                    }
                );

            this.observer.observe(
                this.element.nativeElement
            );
        });
    }

    private disconnect(): void {

        this.observer?.disconnect();

        this.observer = undefined;
    }
}