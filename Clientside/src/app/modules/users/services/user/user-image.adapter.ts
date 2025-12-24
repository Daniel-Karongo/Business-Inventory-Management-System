import { UserService } from '../../services/user/user.service';
import {
  EntityImageAdapter,
  EntityImageAudit
} from '../../../../shared/components/entity-image-manager/entity-image-manager.component';

export const UserImageAdapter = (
  service: UserService
): EntityImageAdapter => ({

  listImages: (id: string) =>
    service.listImages(id),

  listImageAudits: (id: string) =>
    service.imageAuditTarget(id),

  getImageBlob: (id: string, fileName: string, deleted?: boolean) =>
    service.getUserImageBlob(id, fileName, deleted ?? false),

  uploadImages: (id: string, files) =>
    service.uploadImages(id, files),

  softDeleteImage: (id: string, fileName: string) =>
    service.softDeleteImage(id, fileName),

  restoreImage: (id: string, fileName: string) =>
    service.restoreImage(id, fileName),

  hardDeleteImage: (id: string, fileName: string) =>
    service.hardDeleteImage(id, fileName)
});