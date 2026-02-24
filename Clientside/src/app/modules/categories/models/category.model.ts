export interface Category {
  id: number;
  name: string;
  description?: string;
  deleted: boolean;
  parentId?: number;
  parentName?: string | null;

  subcategories?: Category[];
  suppliers?: {
    id: string;
    name: string;
  }[];
}

export interface CategoryFlat extends Category {
  parentName?: string | null;
}

export interface CategoryWithDepth extends Category {
  depth: number;
}