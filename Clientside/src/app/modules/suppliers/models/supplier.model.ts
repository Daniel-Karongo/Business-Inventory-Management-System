export interface Supplier {
    id: string;
    name: string;
    email: string[];
    phoneNumber: string[];
    address: string;
    region: string;
    rating: number;
    imageUrls: string[];
    deleted: boolean;
    createdAt: string;
    createdById: string;
    createdByUsername: string;
    lastUpdatedById: string;
    lastUpdatedByUsername: string;
}