import { map } from 'rxjs/operators';
import { ProductVariantService } from '../services/product-variant.service';
import { EntityImageAdapter, EntityImageAudit } from '../../../../shared/components/entity-image-manager/entity-image-manager.component';
import { of } from 'rxjs';


export const ProductVariantImageAdapter =
    (service: ProductVariantService): EntityImageAdapter => ({

        listImages: (variantId: string) =>
            service.getImages(variantId).pipe(
                map(urls =>
                    (urls ?? []).map(url => ({
                        fileName: url.split('/').pop()!,
                        image: true
                    }))
                )
            ),

        listImageAudits: (_id: string) =>
            of([] as EntityImageAudit[]),

        getImageBlob: (variantId, fileName) =>
            service.getImageBlob(variantId, fileName),

        uploadImages: () => {
            throw new Error('Upload not allowed from sales');
        },

        softDeleteImage: () => {
            throw new Error('Delete not allowed from sales');
        },

        restoreImage: () => {
            throw new Error('Restore not allowed from sales');
        },

        hardDeleteImage: () => {
            throw new Error('Hard delete not allowed from sales');
        }
    });