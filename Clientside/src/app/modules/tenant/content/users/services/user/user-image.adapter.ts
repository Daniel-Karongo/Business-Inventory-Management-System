import { EntityImageAdapter } from '../../../../../../shared/components/entity-image-manager/entity-image-manager.component';
import { USER_DOCUMENT_DELETE_REASONS, USER_DOCUMENT_PERMANENT_DELETE_REASONS, USER_DOCUMENT_RESTORE_REASONS } from './user-reasons.constants';
import { UserService } from './user.service';


export const UserImageAdapter = (
  service: UserService,
  onThumbnailUpdated?: () => void,
  onChange?: () => void
): EntityImageAdapter => ({

  listImages: (id: string) =>
    service.listImages(id),

  listImageAudits: (id: string) =>
    service.imageAuditTarget(id),

  getImageBlob: (id: string, fileName: string, deleted?: boolean) =>
    service.getUserImageBlob(id, fileName, deleted ?? false),

  uploadImages: (id: string, files) =>
    service.uploadImages(id, files),

  setProfileThumbnail: (
    id: string,
    fileName: string
  ) =>
    service.setProfileThumbnail(
      id,
      fileName
    ),

  updateDescription: (id, fileName, description) =>
    service.updateImageDescription(id, fileName, description),

  softDeleteImage: (
    id: string,
    fileName: string,
    reason?: string | null
  ) =>
    service.softDeleteImage(
      id,
      fileName,
      reason
    ),

  restoreImage: (
    id: string,
    fileName: string,
    reason?: string | null
  ) =>
    service.restoreImage(
      id,
      fileName,
      reason
    ),

  hardDeleteImage: (
    id: string,
    fileName: string,
    reason?: string | null
  ) =>
    service.hardDeleteImage(
      id,
      fileName,
      reason
    ),

  onThumbnailUpdated,
  onChange,

  deleteReasons:
    USER_DOCUMENT_DELETE_REASONS,

  restoreReasons:
    USER_DOCUMENT_RESTORE_REASONS,

  hardDeleteReasons:
    USER_DOCUMENT_PERMANENT_DELETE_REASONS,

  supportsDescription: true,

  entityLabel: 'Document',

  uploadMode: 'document',
  descriptionOptions: [
    'ID',
    'Passport',
    'Signature',
    'CV',
    'Contract',
    'Other'
  ],
});