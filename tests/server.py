from http.server import BaseHTTPRequestHandler, HTTPServer
import json

# Sample model list (simulating Ollama's `/api/tags` route)
available_models = [
    {"name": "llama3.2:3b", "size": "3B"},
    {"name": "llama3.2:3b", "size": "1B"}
]

class SimpleHTTPRequestHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        """Handle GET request to return a list of models"""
        self.send_response(200)  # Send HTTP 200 OK
        self.send_header('Content-Type', 'application/json')
        self.end_headers()
        
        response = {"models": available_models}
        self.wfile.write(json.dumps(response).encode("utf-8"))

    def do_POST(self):
        """Handle POST request to receive and print JSON data"""
        content_length = int(self.headers.get('Content-Length', 0))  # Get request size
        post_data = self.rfile.read(content_length)  # Read request body
        
        try:
            json_data = json.loads(post_data)  # Parse JSON
            print("Received JSON:", json.dumps(json_data, indent=2))
            response = {"status": "ok"}
        except json.JSONDecodeError:
            response = {"error": "Invalid JSON"}
            self.send_response(400)  # HTTP 400 Bad Request
        else:
            self.send_response(200)  # HTTP 200 OK

        self.send_header('Content-Type', 'application/json')
        self.end_headers()
        self.wfile.write(json.dumps(response).encode("utf-8"))

if __name__ == "__main__":
    server_address = ('', 11434)
    httpd = HTTPServer(server_address, SimpleHTTPRequestHandler)
    httpd.serve_forever()
