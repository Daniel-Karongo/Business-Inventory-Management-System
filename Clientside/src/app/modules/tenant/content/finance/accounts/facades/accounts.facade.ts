import {
    Injectable,
    computed,
    inject
} from '@angular/core';

import {
    AccountsApiAdapter
} from '../adapters/accounts-api.adapter';

import {
    AccountsState
} from '../state/accounts.state';

import {
    TableQuery
} from '../models/state/table-query.model';

import {
    FacadeRequestUtils
} from '../../../../../../core/utils/facade-request.utils';

@Injectable({
    providedIn: 'root'
})
export class AccountsFacade {

    private readonly api =
        inject(AccountsApiAdapter);

    private readonly store =
        inject(AccountsState);

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
        query: TableQuery,
        branchId?: string
    ): void {

        FacadeRequestUtils.handle(
            this.store.request,
            this.api.list(
                query,
                branchId
            ),
            result => {
                this.store.data.set(result);
            },
            'Failed to load accounts'
        );
    }

}