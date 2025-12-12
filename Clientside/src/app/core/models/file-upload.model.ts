export interface FileUploadDTO {
  file: File;           // matches MultipartFile
  description?: string; // optional string
}