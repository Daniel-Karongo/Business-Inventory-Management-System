import {
    Injectable,
    inject
} from '@angular/core';

import {
    HttpClient
} from '@angular/common/http';

import {
    Observable
} from 'rxjs';

import {
    environment
} from '../../../environments/environment';

import {
    SessionLimitInfoRequest,
    SessionLimitInfoResponse,
    SessionLogoutRequest,
    SessionRecoveryRequest,
    UserSessionDto
} from '../models/session.models';

@Injectable({
    providedIn: 'root'
})
export class SessionService {

    private http = inject(HttpClient);

    getActiveSessions(): Observable<UserSessionDto[]> {
        return this.http.post<UserSessionDto[]>(
            `${environment.apiUrl}/auth/sessions`,
            {},
            {
                withCredentials: true
            }
        );
    }

    getSessionCount(): Observable<number> {
        return this.http.get<number>(
            `${environment.apiUrl}/auth/sessions/count`,
            {
                withCredentials: true
            }
        );
    }

    logoutSession(
        tokenId: string
    ): Observable<void> {

        const body: SessionLogoutRequest = {
            tokenId
        };

        return this.http.post<void>(
            `${environment.apiUrl}/auth/sessions/logout`,
            body,
            {
                withCredentials: true
            }
        );
    }

    logoutOtherSessions(): Observable<void> {
        return this.http.post<void>(
            `${environment.apiUrl}/auth/sessions/logout-others`,
            {},
            {
                withCredentials: true
            }
        );
    }

    getSessionLimitInfo(
        request: SessionLimitInfoRequest
    ): Observable<SessionLimitInfoResponse> {

        return this.http.post<SessionLimitInfoResponse>(
            `${environment.apiUrl}/auth/session-limit-info`,
            request
        );
    }

    logoutRecoverySession(
        request: SessionRecoveryRequest
    ): Observable<void> {

        return this.http.post<void>(
            `${environment.apiUrl}/auth/session-recovery/logout`,
            request
        );
    }
}