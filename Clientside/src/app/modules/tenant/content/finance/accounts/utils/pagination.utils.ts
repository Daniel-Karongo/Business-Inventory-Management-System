import { PageWrapper } from "../../../../../../core/models/page-wrapper.model";

export class PaginationUtils {
    static empty<T>(): PageWrapper<T> {
        return {
            content: [],
            pageNumber: 0,
            pageSize: 25,
            totalElements: 0,
            totalPages: 0,
            last: true
        };
    }
}