import {
    Injectable,
    computed,
    inject
} from '@angular/core';

import {
    JournalApiAdapter
} from '../adapters/journal-api.adapter';

import {
    JournalState
} from '../state/journal.state';

import {
    TableQuery
} from '../models/state/table-query.model';

import {
    FacadeRequestUtils
} from '../../../../../../core/utils/facade-request.utils';

@Injectable({
    providedIn: 'root'
})
export class JournalFacade {

    private readonly api =
        inject(JournalApiAdapter);

    private readonly store =
        inject(JournalState);

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
        query: TableQuery
    ): void {

        FacadeRequestUtils.handle(
            this.store.request,

            this.api.list(query),

            result => {
                this.store.data.set(result);
            },

            'Failed to load journals'
        );
    }

}