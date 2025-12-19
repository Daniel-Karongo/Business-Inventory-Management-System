export interface Category {
    id: string;
    name: string;
    description?: string;
    deleted: boolean;
    parentId?: number; 
    subcategories?: Category[];
    suppliersIds?: string[];
    suppliers?: {
        id: string;
        name: string;
    }[];
}