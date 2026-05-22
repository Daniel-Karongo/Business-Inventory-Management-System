import {
    Injectable,
    computed,
    inject
} from '@angular/core';

import {
    LedgerApiAdapter
} from '../adapters/ledger-api.adapter';

import {
    LedgerState
} from '../state/ledger.state';

import {
    TableQuery
} from '../models/state/table-query.model';

import {
    FacadeRequestUtils
} from '../../../../../../core/utils/facade-request.utils';

@Injectable({
    providedIn: 'root'
})
export class LedgerFacade {

    private readonly api =
        inject(LedgerApiAdapter);

    private readonly store =
        inject(LedgerState);

    readonly data =
        computed(() =>
            this.store.data().content
        );

    readonly page =
        computed(() =>
            this.store.data()
        );

    readonly request =
        this.store.request;

    load(
        accountId: string,
        query: TableQuery
    ): void {

        FacadeRequestUtils.handle(
            this.store.request,

            this.api.list(
                accountId,
                query
            ),

            result => {
                this.store.data.set(result);
            },

            'Failed to load ledger'
        );
    }

}