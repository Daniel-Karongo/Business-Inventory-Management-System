module.exports = {
  '/api': {
    target: 'http://localhost:8080',
    secure: false,
    changeOrigin: true,
    xfwd: true,
    router: (req) => {
      const host = req.headers.host.split(':')[0];
      return `http://${host}:8080`;
    },
    onProxyReq: (proxyReq, req) => {
      const host = req.headers.host;
      proxyReq.setHeader('Host', host);
    },
    logLevel: 'debug'
  }
};