import { BranchMinimalDTO } from "../../branches/models/branch.model";
import { MinimalUserDTO } from "../../users/models/user.model";

export interface DepartmentDTO {
  id?: string; // read-only
  name: string;
  description?: string;
  rollcallStartTime?: string; // LocalTime → string
  gracePeriodMinutes?: number;

  // write-only to backend
  headIds?: string[];
  memberIds?: string[];
  branchIds?: string[];

  // read-only from backend
  heads?: MinimalUserDTO[];
  members?: MinimalUserDTO[];
  branches?: BranchMinimalDTO[];

  deleted?: boolean; // read-only
}

export interface DepartmentMinimalDTO {
  id: string;
  name: string;
}

export interface DepartmentAssignmentDTO {
  branchId: string;
  departmentId: string;
  position: "head" | "member";
}

export interface DepartmentPositionDTO {
  departmentId: string;
  departmentName: string;
  position: "head" | "member";
}
