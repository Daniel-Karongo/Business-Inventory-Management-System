export interface TableQuery {
  page: number;
  size: number;
  sort?: string;

  search?: string;
  type?: string;
  active?: boolean;
  balanceFilter?: string;
}