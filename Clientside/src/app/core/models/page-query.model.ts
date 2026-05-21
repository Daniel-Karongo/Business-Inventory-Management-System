export interface PageQuery {
  page?: number;
  size?: number;

  sortBy?: string;

  direction?: 'asc' | 'desc';

  search?: string;

  hasOverdue?: boolean;

  hasUnapplied?: boolean;
}