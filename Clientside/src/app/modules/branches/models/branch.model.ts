import { DepartmentMinimalDTO, DepartmentPositionDTO } from "../../departments/models/department.model";
import { MinimalUserDTO } from "../../users/models/user.model";

export interface BranchDTO {
  id?: string; // read only
  branchCode?: string;
  name: string;
  location?: string;
  phone?: string;
  email?: string;

  // write-only
  userIds?: string[];
  departmentIds?: string[];

  // read-only
  users?: MinimalUserDTO[];
  departments?: DepartmentMinimalDTO[];
  deleted?: boolean;
}

export interface BranchHierarchyDTO {
  branchId: string;
  branchName: string;
  departments: DepartmentPositionDTO[];
}

export interface BranchMinimalDTO {
  id: string;
  branchCode: string;
  name: string;
}
