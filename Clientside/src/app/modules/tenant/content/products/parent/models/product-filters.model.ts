export interface ProductFilters {
  name?: string;
  description?: string;
  categoryIds?: number[];
  minPrice?: number;
  maxPrice?: number;
  includeDeleted?: boolean;
}