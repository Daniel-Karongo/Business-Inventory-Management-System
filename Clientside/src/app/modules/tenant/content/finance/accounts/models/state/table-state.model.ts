import { PageWrapper } from "../../../../../../../core/models/page-wrapper.model";
import { RequestState } from "../../../../../../../core/models/request-state.model";

export interface TableState<T> {
    data: PageWrapper<T>;
    request: RequestState;
}