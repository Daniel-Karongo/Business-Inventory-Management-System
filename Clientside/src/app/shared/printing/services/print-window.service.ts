import {
    ApplicationRef,
    ComponentRef,
    EnvironmentInjector,
    Injectable,
    Injector,
    Type,
    createComponent
} from '@angular/core';

@Injectable({
    providedIn: 'root'
})
export class PrintWindowService {

    constructor(
        private readonly appRef: ApplicationRef,
        private readonly injector: Injector,
        private readonly environmentInjector: EnvironmentInjector
    ) { }

    async printComponent<T extends object>(
        component: Type<T>,
        inputs?: Partial<T>,
        title?: string
    ): Promise<void> {

        const iframe = this.createPrintFrame();

        let componentRef: ComponentRef<T> | null = null;

        try {

            const iframeDocument =
                iframe.contentDocument;

            const iframeWindow =
                iframe.contentWindow;

            if (!iframeDocument || !iframeWindow) {
                throw new Error(
                    'Unable to access print iframe.'
                );
            }

            iframeDocument.open();

            iframeDocument.write(`
                <!DOCTYPE html>
                <html lang="en">
                    <head>
                        <meta charset="utf-8" />
                        <base href="${document.baseURI}" />
                        <title>${title ?? 'Print'}</title>
                    </head>

                    <body class="print-root">
                    </body>
                </html>
            `);

            iframeDocument.close();

            /*
             * =====================================================
             * LOAD GLOBAL STYLESHEETS
             * =====================================================
             */

            this.cloneGlobalStylesheets(
                iframeDocument
            );

            /*
             * =====================================================
             * INLINE GLOBAL STYLE TAGS
             * =====================================================
             */

            this.cloneGlobalStyleTags(
                iframeDocument
            );

            /*
             * =====================================================
             * PRINT BASELINE
             * =====================================================
             */

            const baseStyle =
                iframeDocument.createElement('style');

            baseStyle.textContent = `
                html,
                body {
                    margin: 0;
                    padding: 0;
                    width: 100%;
                    min-height: 100%;
                    background: white;
                }

                body {
                    display: block;
                }

                * {
                    box-sizing: border-box;
                    -webkit-print-color-adjust: exact;
                    print-color-adjust: exact;
                }

                @page {
                    margin: 0;
                }
            `;

            iframeDocument.head.appendChild(
                baseStyle
            );

            /*
             * =====================================================
             * HOST
             * =====================================================
             */

            const hostElement =
                iframeDocument.createElement('div');

            hostElement.className =
                'print-host';

            iframeDocument.body.appendChild(
                hostElement
            );

            /*
             * =====================================================
             * CREATE COMPONENT
             * =====================================================
             */

            componentRef = createComponent(component, {
                environmentInjector:
                    this.environmentInjector,

                elementInjector:
                    this.injector,

                hostElement
            });

            if (inputs) {
                Object.assign(
                    componentRef.instance,
                    inputs
                );
            }

            this.appRef.attachView(
                componentRef.hostView
            );

            /*
             * =====================================================
             * CRITICAL:
             * FORCE COMPONENT STYLE MIGRATION
             * =====================================================
             */

            await this.waitForRender();
            await this.waitForRender();

            this.migrateAngularRuntimeStyles(
                iframeDocument
            );

            await this.waitForRender();

            await this.waitForFonts(
                iframeDocument
            );

            await this.waitForImages(
                iframeDocument
            );

            iframeWindow.focus();

            await this.printFrame(
                iframeWindow
            );

        } finally {

            if (componentRef) {

                this.appRef.detachView(
                    componentRef.hostView
                );

                componentRef.destroy();
            }

            await this.delay(150);

            iframe.remove();
        }
    }

    /*
     * =========================================================
     * GLOBAL LINK STYLESHEETS
     * =========================================================
     */

    private cloneGlobalStylesheets(
        iframeDocument: Document
    ): void {

        const links = Array.from(
            document.querySelectorAll(
                'link[rel="stylesheet"]'
            )
        );

        for (const link of links) {

            const href =
                link.getAttribute('href');

            if (!href) {
                continue;
            }

            const clone =
                iframeDocument.createElement('link');

            clone.rel = 'stylesheet';

            clone.href =
                new URL(
                    href,
                    document.baseURI
                ).toString();

            iframeDocument.head.appendChild(
                clone
            );
        }
    }

    /*
     * =========================================================
     * GLOBAL INLINE STYLE TAGS
     * =========================================================
     */

    private cloneGlobalStyleTags(
        iframeDocument: Document
    ): void {

        const styles = Array.from(
            document.querySelectorAll('style')
        );

        for (const style of styles) {

            const clone =
                iframeDocument.createElement('style');

            clone.textContent =
                style.textContent;

            iframeDocument.head.appendChild(
                clone
            );
        }
    }

    /*
     * =========================================================
     * ANGULAR RUNTIME STYLE MIGRATION
     * =========================================================
     */

    private migrateAngularRuntimeStyles(
        iframeDocument: Document
    ): void {

        const runtimeStyles =
            Array.from(
                document.querySelectorAll('style')
            )
                .filter(style => {

                    const text =
                        style.textContent ?? '';

                    return (
                        text.includes('invoice-')
                        || text.includes('thermal-')
                        || text.includes('print-')
                    );
                });

        for (const style of runtimeStyles) {

            const clone =
                iframeDocument.createElement('style');

            clone.textContent =
                style.textContent;

            iframeDocument.head.appendChild(
                clone
            );
        }
    }

    /*
     * =========================================================
     * PRINT FRAME
     * =========================================================
     */

    private async printFrame(
        iframeWindow: Window
    ): Promise<void> {

        return new Promise<void>(resolve => {

            let completed = false;

            const cleanup = (): void => {

                if (completed) {
                    return;
                }

                completed = true;

                iframeWindow.onafterprint = null;

                resolve();
            };

            iframeWindow.onafterprint =
                cleanup;

            setTimeout(() => {

                iframeWindow.print();

            }, 120);

            setTimeout(() => {

                cleanup();

            }, 2000);
        });
    }

    /*
     * =========================================================
     * FRAME
     * =========================================================
     */

    private createPrintFrame(): HTMLIFrameElement {

        const iframe =
            document.createElement('iframe');

        iframe.style.position = 'fixed';
        iframe.style.right = '0';
        iframe.style.bottom = '0';
        iframe.style.width = '0';
        iframe.style.height = '0';
        iframe.style.border = '0';
        iframe.style.opacity = '0';
        iframe.style.pointerEvents = 'none';

        document.body.appendChild(
            iframe
        );

        return iframe;
    }

    /*
     * =========================================================
     * FONTS
     * =========================================================
     */

    private async waitForFonts(
        documentRef: Document
    ): Promise<void> {

        const fonts =
            (documentRef as Document & {
                fonts?: FontFaceSet;
            }).fonts;

        if (!fonts) {
            return;
        }

        try {

            await fonts.ready;

        } catch {
        }
    }

    /*
     * =========================================================
     * IMAGES
     * =========================================================
     */

    private async waitForImages(
        documentRef: Document
    ): Promise<void> {

        const images =
            Array.from(
                documentRef.images
            );

        await Promise.all(
            images.map(image => {

                if (image.complete) {
                    return Promise.resolve();
                }

                return new Promise<void>(resolve => {

                    image.onload =
                        () => resolve();

                    image.onerror =
                        () => resolve();
                });
            })
        );
    }

    /*
     * =========================================================
     * RENDER
     * =========================================================
     */

    private waitForRender(): Promise<void> {

        return new Promise(resolve => {

            requestAnimationFrame(() => {

                requestAnimationFrame(() => {

                    resolve();
                });
            });
        });
    }

    /*
     * =========================================================
     * DELAY
     * =========================================================
     */

    private delay(
        milliseconds: number
    ): Promise<void> {

        return new Promise(resolve => {

            setTimeout(
                resolve,
                milliseconds
            );
        });
    }
}