import { Injectable } from '@angular/core';

@Injectable({ providedIn: 'root' })
export class WebAuthnService {

  async register(options: any): Promise<any> {

    const publicKey = this.preformatCreate(options);

    const credential = await navigator.credentials.create({
      publicKey
    }) as PublicKeyCredential;

    return this.postformatCreate(credential);
  }

  private preformatCreate(options: any): PublicKeyCredentialCreationOptions {
    return {
      ...options,
      challenge: this.base64ToArrayBuffer(options.challenge),
      user: {
        ...options.user,
        id: this.base64ToArrayBuffer(options.user.id)
      }
    };
  }

  private postformatCreate(cred: PublicKeyCredential) {
    const response = cred.response as AuthenticatorAttestationResponse;

    return {
      id: cred.id,
      rawId: this.arrayBufferToBase64(cred.rawId),
      type: cred.type,
      response: {
        attestationObject: this.arrayBufferToBase64(response.attestationObject),
        clientDataJSON: this.arrayBufferToBase64(response.clientDataJSON)
      }
    };
  }

  async authenticate(options: any): Promise<any> {

    const publicKey = this.preformatGet(options);

    const credential = await navigator.credentials.get({
      publicKey
    }) as PublicKeyCredential;

    return this.postformatGet(credential);
  }

  // ===== FORMATTERS =====

  private preformatGet(options: any): PublicKeyCredentialRequestOptions {
    return {
      ...options,
      challenge: this.base64ToArrayBuffer(options.challenge),
      allowCredentials: options.allowCredentials?.map((c: any) => ({
        ...c,
        id: this.base64ToArrayBuffer(c.id)
      }))
    };
  }

  private postformatGet(cred: PublicKeyCredential) {
    const response = cred.response as AuthenticatorAssertionResponse;

    return {
      id: cred.id,
      rawId: this.arrayBufferToBase64(cred.rawId),
      type: cred.type,
      response: {
        authenticatorData: this.arrayBufferToBase64(response.authenticatorData),
        clientDataJSON: this.arrayBufferToBase64(response.clientDataJSON),
        signature: this.arrayBufferToBase64(response.signature),
        userHandle: response.userHandle
          ? this.arrayBufferToBase64(response.userHandle)
          : null
      }
    };
  }

  private base64ToArrayBuffer(base64: string): ArrayBuffer {
    const binary = atob(base64.replace(/-/g, '+').replace(/_/g, '/'));
    const bytes = new Uint8Array(binary.length);
    for (let i = 0; i < binary.length; i++) {
      bytes[i] = binary.charCodeAt(i);
    }
    return bytes.buffer;
  }

  private arrayBufferToBase64(buffer: ArrayBuffer): string {
    const bytes = new Uint8Array(buffer);
    let binary = '';
    bytes.forEach(b => binary += String.fromCharCode(b));
    return btoa(binary).replace(/\+/g, '-').replace(/\//g, '_').replace(/=/g, '');
  }
}