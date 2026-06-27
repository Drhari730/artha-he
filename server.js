// Minimal zero-dependency static file server for Artha HE.
// Serves the current directory; Railway/Heroku/Render set process.env.PORT.
const http = require("http");
const fs = require("fs");
const path = require("path");

const PORT = process.env.PORT || 8088;
const ROOT = __dirname;

const TYPES = {
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".csv": "text/csv; charset=utf-8",
  ".svg": "image/svg+xml",
  ".png": "image/png",
  ".ico": "image/x-icon",
  ".map": "application/json"
};

const server = http.createServer((req, res) => {
  // Strip query string, prevent path traversal.
  let urlPath = decodeURIComponent((req.url || "/").split("?")[0]);
  if (urlPath === "/" || urlPath === "") urlPath = "/index.html";
  const filePath = path.normalize(path.join(ROOT, urlPath));
  if (!filePath.startsWith(ROOT)) {
    res.writeHead(403).end("Forbidden");
    return;
  }
  fs.readFile(filePath, (err, data) => {
    if (err) {
      // SPA-style fallback to index.html for unknown routes.
      fs.readFile(path.join(ROOT, "index.html"), (e2, home) => {
        if (e2) { res.writeHead(404).end("Not found"); return; }
        res.writeHead(200, { "Content-Type": TYPES[".html"] }).end(home);
      });
      return;
    }
    const type = TYPES[path.extname(filePath).toLowerCase()] || "application/octet-stream";
    res.writeHead(200, { "Content-Type": type }).end(data);
  });
});

server.listen(PORT, () => console.log(`Artha HE running on port ${PORT}`));
