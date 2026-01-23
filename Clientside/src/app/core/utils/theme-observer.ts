export function observeThemeChange(callback: () => void): MutationObserver {
  const observer = new MutationObserver(() => callback());

  observer.observe(document.body, {
    attributes: true,
    attributeFilter: ['class']
  });

  return observer;
}