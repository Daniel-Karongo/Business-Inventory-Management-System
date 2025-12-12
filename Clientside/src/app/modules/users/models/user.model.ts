import { FileUploadDTO } from "../../../core/models/file-upload.model";
import { BranchHierarchyDTO } from "../../branches/models/branch.model";
import { DepartmentAssignmentDTO, DepartmentMinimalDTO } from "../../departments/models/department.model";

export interface User {
  id?: string; // read only

  username: string;
  password?: string;
  emailAddresses?: string[];
  phoneNumbers?: string[];
  idNumber?: string;
  role: string;

  // read-only
  branchHierarchy?: BranchHierarchyDTO[];
  departments?: DepartmentMinimalDTO[];

  // write-only
  departmentsAndPositions?: DepartmentAssignmentDTO[];

  createdBy?: string;
  lastModifiedBy?: string;
  createdAt?: string; // LocalDateTime
  lastModifiedAt?: string;

  deleted?: boolean;

  // write-only uploads
  userFiles?: FileUploadDTO[];

  // read-only
  idImageUrls?: string[];
}

export interface MinimalUserDTO {
  id: string;
  username: string;
}
