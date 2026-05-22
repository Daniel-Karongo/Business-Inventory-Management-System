import {
    WritableSignal
} from '@angular/core';

import {
    catchError,
    finalize,
    Observable,
    of
} from 'rxjs';

import {
    RequestState
} from '../models/request-state.model';

export class FacadeRequestUtils {

    static handle<T>(
        request:
            WritableSignal<RequestState>,

        operation:
            Observable<T>,

        onSuccess:
            (result: T) => void,

        fallbackMessage:
            string
    ): void {

        request.update(state => ({
            ...state,
            loading: true,
            loaded: false,
            error: null
        }));

        operation
            .pipe(
                catchError(err => {

                    request.update(state => ({
                        ...state,
                        loading: false,
                        loaded: true,
                        error:
                            err?.error?.message
                            ??
                            fallbackMessage
                    }));

                    return of(null);
                }),
                finalize(() => {

                    request.update(state => ({
                        ...state,
                        loading: false
                    }));
                })
            )
            .subscribe(result => {

                if (!result) {
                    return;
                }

                request.update(state => ({
                    ...state,
                    loaded: true,
                    error: null
                }));

                onSuccess(result);
            });
    }

}