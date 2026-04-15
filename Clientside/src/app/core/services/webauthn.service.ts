import { Injectable } from '@angular/core';

@Injectable({ providedIn: 'root' })
export class WebAuthnService {

  /* =========================
     REGISTRATION
  ========================= */

  async register(options: any): Promise<any> {

    this.ensureSupported();

    const publicKey = this.prepareCreationOptions(options);

    const credential = await navigator.credentials.create({
      publicKey
    }) as PublicKeyCredential;

    return this.serializeAttestation(credential);
  }

  private prepareCreationOptions(opts: any): PublicKeyCredentialCreationOptions {

    const extensions = { ...(opts.extensions || {}) };

    if (extensions.appidExclude == null) {
      delete extensions.appidExclude;
    }

    return {
      ...opts,

      challenge: this.base64urlToBuffer(opts.challenge),

      user: {
        ...opts.user,
        id: this.base64urlToBuffer(opts.user.id)
      },

      excludeCredentials: (opts.excludeCredentials || []).map((c: any) => ({
        ...c,
        id: this.base64urlToBuffer(c.id)
      })),

      timeout: opts.timeout ?? 60000
    };
  }

  /* =========================
     AUTHENTICATION
  ========================= */

  async authenticate(options: any): Promise<any> {

    this.ensureSupported();

    const publicKey = this.prepareRequestOptions(options);

    const credential = await navigator.credentials.get({
      publicKey
    }) as PublicKeyCredential;

    return this.serializeAssertion(credential);
  }

  private prepareRequestOptions(opts: any): PublicKeyCredentialRequestOptions {

    return {
      ...opts,

      challenge: this.base64urlToBuffer(opts.challenge),

      allowCredentials: (opts.allowCredentials || []).map((c: any) => ({
        ...c,
        id: this.base64urlToBuffer(c.id)
      })),

      timeout: opts.timeout ?? 60000
    };
  }

  /* =========================
     SERIALIZATION
  ========================= */

  private serializeAttestation(cred: PublicKeyCredential) {

    const res = cred.response as AuthenticatorAttestationResponse;

    return {
      id: cred.id,
      rawId: this.bufferToBase64url(cred.rawId),
      type: cred.type,
      response: {
        attestationObject: this.bufferToBase64url(res.attestationObject),
        clientDataJSON: this.bufferToBase64url(res.clientDataJSON)
      },

      clientExtensionResults: {}
    };
  }

  private serializeAssertion(cred: PublicKeyCredential) {

    const res = cred.response as AuthenticatorAssertionResponse;

    return {
      id: cred.id,
      rawId: this.bufferToBase64url(cred.rawId),
      type: cred.type,
      response: {
        authenticatorData: this.bufferToBase64url(res.authenticatorData),
        clientDataJSON: this.bufferToBase64url(res.clientDataJSON),
        signature: this.bufferToBase64url(res.signature),
        userHandle: res.userHandle
          ? this.bufferToBase64url(res.userHandle)
          : null
      }
    };
  }

  /* =========================
     HELPERS (CRITICAL)
  ========================= */

  private ensureSupported() {
    if (!window.PublicKeyCredential) {
      throw new Error('WebAuthn not supported');
    }
  }

  private base64urlToBuffer(base64url: string): ArrayBuffer {

    const base64 = base64url
      .replace(/-/g, '+')
      .replace(/_/g, '/')
      .padEnd(Math.ceil(base64url.length / 4) * 4, '=');

    const binary = atob(base64);
    const bytes = new Uint8Array(binary.length);

    for (let i = 0; i < binary.length; i++) {
      bytes[i] = binary.charCodeAt(i);
    }

    return bytes.buffer;
  }

  private bufferToBase64url(buffer: ArrayBuffer): string {

    const bytes = new Uint8Array(buffer);
    let binary = '';

    bytes.forEach(b => binary += String.fromCharCode(b));

    return btoa(binary)
      .replace(/\+/g, '-')
      .replace(/\//g, '_')
      .replace(/=/g, '');
  }
}