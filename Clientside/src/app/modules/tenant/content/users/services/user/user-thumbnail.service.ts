import { Injectable } from '@angular/core';
import { firstValueFrom, from, Observable, of, shareReplay, switchMap } from 'rxjs';
import * as pdfjsLib from 'pdfjs-dist';
import { UserService } from '../user/user.service';

@Injectable({ providedIn: 'root' })
export class UserThumbnailService {

    private cache = new Map<string, Observable<string>>();

    constructor(private userService: UserService) { }

    getThumbnail(userId: string, url: string): Observable<string> {
        if (!url) return of('');

        const key = `${userId}_${url}`;

        if (!this.cache.has(key)) {
            const fileName = url.split('/').pop()!;
            const ext = fileName.split('.').pop()?.toLowerCase();

            const obs$ = this.userService
                .getUserImageBlob(userId, fileName, null)
                .pipe(
                    switchMap(blob => {
                        if (['jpg', 'jpeg', 'png', 'gif', 'webp'].includes(ext!)) {
                            return of(URL.createObjectURL(blob));
                        }

                        if (ext === 'pdf') {
                            return from(this.renderPdf(blob));
                        }

                        return of('');
                    }),
                    shareReplay({ bufferSize: 1, refCount: true })
                );

            this.cache.set(key, obs$);
        }

        return this.cache.get(key)!;
    }

    private async renderPdf(blob: Blob): Promise<string> {
        const pdf = await pdfjsLib.getDocument({
            data: await blob.arrayBuffer()
        }).promise;

        const page = await pdf.getPage(1);
        const viewport = page.getViewport({ scale: 0.5 });

        const canvas = document.createElement('canvas');
        const ctx = canvas.getContext('2d');
        if (!ctx) return '';

        canvas.width = viewport.width;
        canvas.height = viewport.height;

        await page.render({ canvasContext: ctx, viewport } as any).promise;

        return canvas.toDataURL();
    }

    invalidate(userId: string) {
        [...this.cache.entries()]
            .filter(([k]) => k.startsWith(userId + '_'))
            .forEach(([k, obs$]) => {
                obs$.subscribe(url => {
                    if (url.startsWith('blob:')) {
                        URL.revokeObjectURL(url); // 🔥 CLEANUP
                    }
                }).unsubscribe();

                this.cache.delete(k);
            });
    }

    clearAll() {
        this.cache.clear();
    }
}