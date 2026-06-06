import { EntityImageAdapter } from '../../../../../shared/components/entity-image-manager/entity-image-manager.component';
import { BranchService } from '../services/branch.service';

export const BranchDocumentAdapter = (
  service: BranchService,
  onChange?: () => void
): EntityImageAdapter => ({
  listImages: id =>
    service.listDocuments(id),

  listImageAudits: id =>
    service.listDocumentAudits(id),

  getImageBlob: (
    id,
    fileName
  ) =>
    service.downloadDocument(
      id,
      fileName
    ),

  uploadImages: (
    id,
    files
  ) =>
    service.uploadDocuments(
      id,
      files
    ),

  updateDescription: (
    id,
    fileName,
    description
  ) =>
    service.updateDocumentDescription(
      id,
      fileName,
      description
    ),

  setProfileThumbnail: (
    id,
    fileName
  ) =>
    service.setBranchLogo(
      id,
      fileName
    ),

  softDeleteImage: (
    id,
    fileName,
    reason
  ) =>
    service.softDeleteDocument(
      id,
      fileName,
      reason
    ),

  restoreImage: (
    id,
    fileName,
    reason
  ) =>
    service.restoreDocument(
      id,
      fileName,
      reason
    ),

  hardDeleteImage: (
    id,
    fileName,
    reason
  ) =>
    service.hardDeleteDocument(
      id,
      fileName,
      reason
    ),

  supportsDescription: true,
  entityLabel: 'Document',
  uploadMode: 'document',
  allowLogo: true,
  onChange,
  onThumbnailUpdated: onChange,
  descriptionOptions: [
    'Business Permit',
    'KRA Certificate',
    'Fire Compliance',
    'Health Certificate',
    'Lease Agreement',
    'Branch Photo',
    'Logo',
    'Other'
  ],
});