export interface PageResponse<T> {
    content: T[];
    pageable: {
        pageNumber: number;
        pageSize: number;
    };
    totalElements: number;
    totalPages: number;
    last: boolean;
    first: boolean;
    size: number;
    number: number;
    empty: boolean;
}

export interface PageRequest {
    page?: number;
    size?: number;
    sort?: string;
}