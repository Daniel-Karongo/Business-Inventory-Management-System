export interface DepartmentDTO {
  id?: string;
  name: string;
  description?: string;
  branchId?: string;
  deleted?: boolean;
}

export interface DepartmentMinimalDTO {
  id: string;
  name: string;
}

export interface DepartmentAssignmentDTO {
  branchId: string;
  departmentId: string;
  position?: string;
}