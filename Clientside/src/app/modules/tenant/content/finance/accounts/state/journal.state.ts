import {
    Injectable,
    signal
} from '@angular/core';

import { Journal }
    from '../models/journal.models';

import {
    PageWrapper
} from '../../../../../../core/models/page-wrapper.model';

import {
    initialRequestState,
    RequestState
} from '../../../../../../core/models/request-state.model';

@Injectable({
    providedIn: 'root'
})
export class JournalState {

    readonly data =
        signal<PageWrapper<Journal>>({
            content: [],
            pageNumber: 0,
            pageSize: 25,
            totalElements: 0,
            totalPages: 0,
            last: true
        });

    readonly request =
        signal<RequestState>(
            initialRequestState()
        );

}