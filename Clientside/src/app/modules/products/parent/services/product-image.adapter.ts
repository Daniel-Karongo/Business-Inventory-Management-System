import { map } from 'rxjs/operators';
import { of } from 'rxjs';

import { ProductService } from '../services/product.service';
import {
  EntityImageAdapter,
  EntityImageAudit
} from '../../../../shared/components/entity-image-manager/entity-image-manager.component';

export const ProductImageAdapter =
  (service: ProductService): EntityImageAdapter => ({

    listImages: (productId: string) =>
      service.getImages(productId).pipe(
        map(urls =>
          (urls ?? []).map(url => ({
            fileName: url.split('/').pop()!,
            image: true
          }))
        )
      ),

    listImageAudits: (_id: string) =>
      of([] as EntityImageAudit[]),

    getImageBlob: (productId, fileName) =>
      service.getImageBlob(productId, fileName),

    uploadImages: () => {
      throw new Error('Upload not allowed from selector');
    },

    softDeleteImage: () => {
      throw new Error('Delete not allowed from selector');
    },

    restoreImage: () => {
      throw new Error('Restore not allowed from selector');
    },

    hardDeleteImage: () => {
      throw new Error('Hard delete not allowed from selector');
    }
  });