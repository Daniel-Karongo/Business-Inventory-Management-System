export interface Category {
  id: number;          // ✅ FIXED
  name: string;
  description?: string;
  deleted: boolean;
  parentId?: number;

  subcategories?: Category[];   // ✅ matches backend tree
  suppliersIds?: string[];
  suppliers?: {
    id: string;
    name: string;
  }[];
}