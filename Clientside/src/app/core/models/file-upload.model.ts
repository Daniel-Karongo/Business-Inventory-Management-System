export interface FileUploadDTO {
  file: File;           // matches MultipartFile
  description?: string; // optional string
}

export interface UploadImagePayload {
  file: File;
  description?: string;
}