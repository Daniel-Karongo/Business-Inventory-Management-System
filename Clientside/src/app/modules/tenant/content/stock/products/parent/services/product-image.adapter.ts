import { map } from 'rxjs/operators';
import { ProductService } from './product.service';
import {
  EntityImageAdapter,
  EntityImageAudit
} from '../../../../../../../shared/components/entity-image-manager/entity-image-manager.component';
import { ProductImageAudit } from '../../../models/product.model';

export const ProductImageAdapter =
  (
    service: ProductService
  ): EntityImageAdapter => ({

    entityLabel: 'Image',

    uploadMode: 'image',

    supportsDescription: false,

    deleteReasons: [
      'Incorrect image',
      'Duplicate image',
      'Obsolete image',
      'Quality issue'
    ],

    restoreReasons: [
      'Image required again',
      'Deleted by mistake'
    ],

    hardDeleteReasons: [
      'Duplicate image',
      'Incorrect upload',
      'Compliance request'
    ],

    listImages: (
      productId: string
    ) =>
      service.getImages(
        productId
      ).pipe(
        map(images =>
          (images ?? []).map(
            img => ({
              id: img.id,
              fileName: img.fileName,
              deleted: img.deleted,
              image: true
            })
          )
        )
      ),

    listImageAudits: (
      productId: string
    ) =>
      service.getImageAudits(
        productId
      ).pipe(
        map(audits =>
          (audits ?? []).map(
            (
              audit: ProductImageAudit
            ): EntityImageAudit => ({

              fileName:
                audit.fileName,

              action:
                audit.action,

              reason:
                audit.reason,

              performedByUsername:
                audit.performedBy,

              timestamp:
                audit.timestamp
            })
          )
        )
      ),

    getImageBlob: (
      productId: string,
      fileName: string,
      deleted?: boolean,
      version?: number
    ) =>
      service.getImageBlob(
        productId,
        fileName,
        undefined,
        version
      ),

    uploadImages: (
      productId: string,
      files: {
        file: File;
        description: string;
      }[]
    ) =>
      service.uploadImages(
        productId,
        files.map(
          x => x.file
        )
      ),

    softDeleteImage: (
      productId: string,
      fileName: string,
      reason?: string | null
    ) =>
      service.deleteImage(
        productId,
        fileName,
        true,
        reason ?? undefined
      ),

    restoreImage: (
      productId,
      imageId,
      reason
    ) =>
      service.restoreImage(
        productId,
        imageId,
        reason ?? undefined
      ),

    hardDeleteImage: (
      productId: string,
      fileName: string,
      reason?: string | null
    ) =>
      service.deleteImage(
        productId,
        fileName,
        false,
        reason ?? undefined
      ),

    onThumbnailUpdated: undefined,
    onChange: undefined
  });