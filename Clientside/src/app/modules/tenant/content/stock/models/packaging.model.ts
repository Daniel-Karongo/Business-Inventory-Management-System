export interface PackagingDTO {
    packagingId: string;

    name: string;

    unitsPerPackaging: number;

    isBaseUnit: boolean;
}

export interface CreatePackagingRequest {
    branchId: string;

    variantId: string;

    name: string;

    unitsPerPackaging: number;
}

export interface UpdatePackagingRequest {
    branchId: string;

    name: string;

    unitsPerPackaging: number;
}