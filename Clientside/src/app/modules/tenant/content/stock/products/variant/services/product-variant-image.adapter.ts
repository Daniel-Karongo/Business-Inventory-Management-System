import { map } from 'rxjs';

import {
    EntityImageAdapter
} from '../../../../../../../shared/components/entity-image-manager/entity-image-manager.component';

import { ProductVariantService } from '../services/product-variant.service';
import { VARIANT_IMAGE_DELETE_REASONS, VARIANT_IMAGE_HARD_DELETE_REASONS, VARIANT_IMAGE_RESTORE_REASONS } from './variant-image-reasons.constants';

export const ProductVariantImageAdapter =
    (
        service: ProductVariantService
    ): EntityImageAdapter => ({

        listImages: (variantId: string) =>
            service.getAllImages(
                variantId
            ).pipe(
                map(images =>
                    (images ?? []).map(img => ({
                        fileName:
                            img.fileName,
                        deleted:
                            img.deleted,
                        image: true
                    }))
                )
            ),

        listImageAudits: (
            variantId: string
        ) =>
            service
                .getImageAuditHistory(
                    variantId
                )
                .pipe(
                    map(audits =>
                        (audits ?? []).map(a => ({
                            fileName:
                                a.fileName,
                            action:
                                a.action,
                            reason:
                                a.reason,
                            performedByUsername:
                                a.performedBy,
                            timestamp:
                                a.timestamp
                        }))
                    )
                ),

        getImageBlob: (
            variantId,
            fileName
        ) =>
            service.getImageBlob(
                variantId,
                fileName
            ),

        uploadImages: (
            variantId,
            files
        ) =>
            service.uploadImages(
                variantId,
                files.map(x => x.file)
            ),

        softDeleteImage: (
            variantId,
            fileName,
            reason
        ) =>
            service.deleteImage(
                variantId,
                fileName,
                reason
            ),

        restoreImage: (
            variantId,
            fileName,
            reason
        ) =>
            service.restoreImage(
                variantId,
                fileName,
                reason
            ),

        hardDeleteImage: (
            variantId,
            fileName,
            reason
        ) =>
            service.hardDeleteImage(
                variantId,
                fileName,
                reason
            ),

        deleteReasons:
            VARIANT_IMAGE_DELETE_REASONS,

        restoreReasons:
            VARIANT_IMAGE_RESTORE_REASONS,

        hardDeleteReasons:
            VARIANT_IMAGE_HARD_DELETE_REASONS,

        supportsDescription: false,

        entityLabel: 'Image',

        uploadMode: 'image',
    });