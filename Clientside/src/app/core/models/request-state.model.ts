export interface RequestState<T> {
  loading: boolean;
  error: string | null;
  data: T | null;
}

export const initialRequestState = <T>(): RequestState<T> => ({
  loading: false,
  error: null,
  data: null
});