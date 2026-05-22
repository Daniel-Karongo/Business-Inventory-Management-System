export interface RequestState {
  loading: boolean;
  loaded: boolean;
  error: string | null;
}

export const initialRequestState =
  (): RequestState => ({
    loading: false,
    loaded: false,
    error: null
  });