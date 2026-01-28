import { Injectable } from "@angular/core";

@Injectable({ providedIn: 'root' })
export class PosBarcodeService {

  async scanViaCamera(): Promise<string | null> {
    if (!('BarcodeDetector' in window)) return null;

    const detector = new (window as any).BarcodeDetector({
      formats: ['code_128', 'ean_13', 'ean_8']
    });

    const stream = await navigator.mediaDevices.getUserMedia({
      video: { facingMode: 'environment' }
    });

    const video = document.createElement('video');
    video.srcObject = stream;
    await video.play();

    return new Promise(resolve => {
      const loop = async () => {
        const codes = await detector.detect(video);
        if (codes.length) {
          stream.getTracks().forEach(t => t.stop());
          resolve(codes[0].rawValue);
        } else {
          requestAnimationFrame(loop);
        }
      };
      loop();
    });
  }
}