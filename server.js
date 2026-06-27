// Artha HE server.
// - Serves ONLY the ./public folder (so engine.js / server.js / package.json are
//   never reachable from the browser).
// - Exposes POST /api/<name> which runs the CONFIDENTIAL engine and returns JSON.
//   The browser sends inputs and receives finished results only.
const http = require("http");
const fs = require("fs");
const path = require("path");
const { COMPUTE } = require("./engine");

const PORT = process.env.PORT || 8088;
const PUBLIC = path.join(__dirname, "public");

const TYPES = {
  ".html": "text/html; charset=utf-8", ".js": "text/javascript; charset=utf-8",
  ".css": "text/css; charset=utf-8", ".json": "application/json; charset=utf-8",
  ".csv": "text/csv; charset=utf-8", ".svg": "image/svg+xml", ".png": "image/png", ".ico": "image/x-icon"
};

function sendJSON(res, code, obj) {
  res.writeHead(code, { "Content-Type": "application/json; charset=utf-8" }).end(JSON.stringify(obj));
}

function handleApi(req, res, name) {
  const fn = COMPUTE[name];
  if (!fn) return sendJSON(res, 404, { error: "unknown endpoint" });
  let body = "";
  req.on("data", chunk => { body += chunk; if (body.length > 5e6) req.destroy(); });
  req.on("end", () => {
    try {
      const payload = body ? JSON.parse(body) : {};
      sendJSON(res, 200, fn(payload));
    } catch (e) {
      sendJSON(res, 400, { error: "computation failed", detail: String(e && e.message || e) });
    }
  });
}

function serveStatic(req, res) {
  let urlPath = decodeURIComponent((req.url || "/").split("?")[0]);
  if (urlPath === "/" || urlPath === "") urlPath = "/index.html";
  const filePath = path.normalize(path.join(PUBLIC, urlPath));
  if (!filePath.startsWith(PUBLIC)) { res.writeHead(403).end("Forbidden"); return; }
  fs.readFile(filePath, (err, data) => {
    if (err) {
      fs.readFile(path.join(PUBLIC, "index.html"), (e2, home) => {
        if (e2) { res.writeHead(404).end("Not found"); return; }
        res.writeHead(200, { "Content-Type": TYPES[".html"] }).end(home);
      });
      return;
    }
    res.writeHead(200, { "Content-Type": TYPES[path.extname(filePath).toLowerCase()] || "application/octet-stream" }).end(data);
  });
}

const server = http.createServer((req, res) => {
  if (req.url && req.url.startsWith("/api/")) {
    if (req.method !== "POST") return sendJSON(res, 405, { error: "POST only" });
    return handleApi(req, res, req.url.slice(5).split("?")[0]);
  }
  serveStatic(req, res);
});

server.listen(PORT, () => console.log(`Artha HE running on port ${PORT} (engine private; public/ served)`));
