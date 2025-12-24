import { map } from 'rxjs/operators';
import { SupplierService } from './supplier.service';
import { EntityImageAdapter, EntityImageAudit } from
    '../../../shared/components/entity-image-manager/entity-image-manager.component';

export const SupplierImageAdapter = (service: SupplierService): EntityImageAdapter => ({
    listImages: (id: string, deleted?: boolean) =>
        deleted !== undefined ?
            service.listImages(id, deleted)
            : service.listImages(id, deleted),

    listImageAudits: id =>
        service.imageAudits(id).pipe(
            map(audits =>
                (audits ?? []).map(a => ({
                    fileName: a.fileName,
                    action: a.action,
                    performedByUsername: a.performedBy ?? 'System',
                    timestamp: a.timestamp
                }) as EntityImageAudit)
            )
        ),

    getImageBlob: (id, file, deleted) =>
        service.getSupplierImageBlob(id, file, deleted),

    uploadImages: (id, files) =>
        service.uploadImages(id, files),

    softDeleteImage: (id, file) =>
        service.softDeleteImage(id, file),

    restoreImage: (id, file) =>
        service.restoreImage(id, file),

    hardDeleteImage: (id, file) =>
        service.hardDeleteImage(id, file)
});
