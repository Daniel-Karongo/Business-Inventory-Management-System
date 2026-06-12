import {
    Injectable
} from '@angular/core';

@Injectable({
    providedIn: 'root'
})
export class ThumbnailCacheService {

    private readonly MAX_PRODUCTS = 1000;
    private readonly MAX_VARIANTS = 250;
    readonly productThumbnails = new Map<string, string>();
    readonly variantThumbnails = new Map<string, string>();
    readonly loadingProducts = new Set<string>();
    readonly loadingVariants = new Set<string>();

    peekProduct(
        id: string
    ): string | undefined {
        return this.productThumbnails.get(id);
    }

    peekVariant(
        id: string
    ): string | undefined {
        return this.variantThumbnails.get(id);
    }

    beginProductLoad(
        id: string
    ): boolean {

        if (
            this.productThumbnails.has(id)
            || this.loadingProducts.has(id)
        ) {
            return false;
        }

        this.loadingProducts.add(id);

        return true;
    }

    finishProductLoad(
        id: string
    ): void {

        this.loadingProducts.delete(
            id
        );
    }

    beginVariantLoad(
        id: string
    ): boolean {

        if (
            this.variantThumbnails.has(id)
            || this.loadingVariants.has(id)
        ) {
            return false;
        }

        this.loadingVariants.add(id);

        return true;
    }

    finishVariantLoad(
        id: string
    ): void {

        this.loadingVariants.delete(
            id
        );
    }

    getProduct(
        id: string
    ): string | undefined {
        const value =
            this.productThumbnails.get(
                id
            );

        if (!value) {
            return undefined;
        }

        this.productThumbnails.delete(
            id
        );

        this.productThumbnails.set(
            id,
            value
        );

        return value;
    }

    setProduct(
        id: string,
        url: string
    ): void {

        const existing =
            this.productThumbnails.get(
                id
            );

        if (existing) {
            URL.revokeObjectURL(
                existing
            );

            this.productThumbnails.delete(
                id
            );
        }

        this.productThumbnails.set(
            id,
            url
        );

        this.evictProducts();
    }

    getVariant(
        id: string
    ): string | undefined {

        const value =
            this.variantThumbnails.get(
                id
            );

        if (!value) {
            return undefined;
        }

        this.variantThumbnails.delete(
            id
        );

        this.variantThumbnails.set(
            id,
            value
        );

        return value;
    }

    setVariant(
        id: string,
        url: string
    ): void {

        const existing =
            this.variantThumbnails.get(
                id
            );

        if (existing) {
            URL.revokeObjectURL(
                existing
            );

            this.variantThumbnails.delete(
                id
            );
        }

        this.variantThumbnails.set(
            id,
            url
        );

        this.evictVariants();
    }

    removeProduct(
        id: string
    ): void {

        const existing =
            this.productThumbnails.get(
                id
            );

        if (existing) {
            URL.revokeObjectURL(
                existing
            );
        }

        this.productThumbnails.delete(
            id
        );
    }

    removeVariant(
        id: string
    ): void {

        const existing =
            this.variantThumbnails.get(
                id
            );

        if (existing) {
            URL.revokeObjectURL(
                existing
            );
        }

        this.variantThumbnails.delete(
            id
        );
    }

    clear(): void {

        this.loadingProducts.clear();

        this.loadingVariants.clear();

        for (
            const url
            of this.productThumbnails.values()
        ) {
            URL.revokeObjectURL(
                url
            );
        }

        for (
            const url
            of this.variantThumbnails.values()
        ) {
            URL.revokeObjectURL(
                url
            );
        }

        this.productThumbnails.clear();

        this.variantThumbnails.clear();
    }

    private evictProducts(): void {

        while (
            this.productThumbnails.size >
            this.MAX_PRODUCTS
        ) {
            const oldest =
                this.productThumbnails
                    .keys()
                    .next()
                    .value;

            if (!oldest) {
                break;
            }

            this.removeProduct(
                oldest
            );
        }
    }

    private evictVariants(): void {

        while (
            this.variantThumbnails.size >
            this.MAX_VARIANTS
        ) {
            const oldest =
                this.variantThumbnails
                    .keys()
                    .next()
                    .value;

            if (!oldest) {
                break;
            }

            this.removeVariant(
                oldest
            );
        }
    }
}