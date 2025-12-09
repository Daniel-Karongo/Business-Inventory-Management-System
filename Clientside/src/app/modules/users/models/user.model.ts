import { BranchHierarchyDTO } from "../../branches/models/branch.model";
import { DepartmentAssignmentDTO, DepartmentMinimalDTO } from "../../departments/models/department.model";

export interface User {
  id?: string;
  username: string;
  password?: string;
  emailAddresses?: string[]; // maps to backend
  phoneNumbers?: string[];
  idNumber?: string;
  role?: string; // single role name
  branchHierarchy?: BranchHierarchyDTO[];
  departments?: DepartmentMinimalDTO[]; // read-only from backend
  departmentsAndPositions?: DepartmentAssignmentDTO[]; // write-only to backend
  createdBy?: string;
  lastModifiedBy?: string;
  createdAt: string;
  lastModifiedAt?: string;
  deleted?: boolean;
  idImageUrls?: string[]; // read-only image URLs
}