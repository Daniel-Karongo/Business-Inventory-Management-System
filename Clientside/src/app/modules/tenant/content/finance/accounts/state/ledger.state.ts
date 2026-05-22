import {
    Injectable,
    signal
} from '@angular/core';

import { LedgerRow }
    from '../models/ledger.models';

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
export class LedgerState {

    readonly data =
        signal<PageWrapper<LedgerRow>>({
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