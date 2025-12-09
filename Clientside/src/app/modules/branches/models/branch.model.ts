export interface BranchDTO {
  id?: string;
  name: string;
  description?: string;
  phone?: string;
  email?: string;
  address?: string;
  deleted?: boolean;
}

export interface BranchHierarchyDTO {
  branchId: string;
  branchName: string;
  departments?: {
    departmentId: string;
    departmentName: string;   // <-- ADD THIS
    position?: string;        // head | member
  }[];
}