export interface TableQuery {
  page: number;
  size: number;
  sortBy?: string;
  direction?: 'asc' | 'desc';
  keyword?: string;
}