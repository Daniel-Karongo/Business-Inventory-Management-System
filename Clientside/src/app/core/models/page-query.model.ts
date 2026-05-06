export interface PageQuery {
  page?: number;
  size?: number;

  sortBy?: string;

  direction?: 'asc' | 'desc';
}