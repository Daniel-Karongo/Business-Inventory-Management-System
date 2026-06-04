export interface UserSessionDto {
    tokenId: string;

    branchId: string | null;

    branchName: string;

    loginDate: string;

    loginTime: string;

    deviceId: string | null;

    deviceName: string | null;

    browserName: string | null;

    osName: string | null;

    platform: string | null;

    ipAddress: string | null;

    current: boolean;

    currentDevice: boolean;
}

export interface SessionLimitInfoRequest {
    identifier: string;

    branchId: string | null;

    deviceId: string;
}

export interface SessionLimitInfoResponse {
    limit: number;

    activeSessions: number;

    sessions: UserSessionDto[];
}

export interface SessionLogoutRequest {
    tokenId: string;
}

export interface SessionRecoveryRequest {
    identifier: string;

    branchId: string | null;

    password: string;

    tokenId: string;
}