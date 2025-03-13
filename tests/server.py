from http.server import BaseHTTPRequestHandler, HTTPServer
import json
import time
import uuid
import re
from urllib.parse import parse_qs, urlparse

# Sample model list (simulating Ollama's `/api/tags` endpoint)
available_models = [
    {"name": "llama:latest", "modified_at": "2023-11-04T14:56:42Z", "size": 3791730638},
    {"name": "tinyllama:latest", "modified_at": "2023-11-06T12:12:15Z", "size": 1220304460},
    {"name": "mistral:latest", "modified_at": "2023-11-10T09:45:22Z", "size": 4180452710},
    {"name": "codellama:latest", "modified_at": "2023-11-08T18:30:05Z", "size": 3962584032}
]

# Mock response for model generation
def generate_streaming_response(model, prompt, stream=True):
    """Generate a mock streaming response similar to Ollama's /api/generate endpoint"""
    model_info = next((m for m in available_models if m["name"] == model), None)
    if not model_info:
        return {"error": f"Model '{model}' not found"}, 404
    
    response_parts = []
    
    # Initial response
    response_parts.append({
        "model": model,
        "created_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "response": "",
        "done": False
    })
    
    # Generate mock responses (5 chunks)
    chunks = [
        "I'll help you with that question. ",
        "Based on the information provided, ",
        "the solution involves several steps. ",
        "First, we need to consider the context. ",
        "Then, we can proceed with implementation."
    ]
    
    # Add content chunks (only if streaming)
    if stream:
        for chunk in chunks:
            response_parts.append({
                "model": model,
                "created_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
                "response": chunk,
                "done": False
            })
    else:
        # For non-streaming, just return the full response
        full_text = "".join(chunks)
        response_parts = [{
            "model": model,
            "created_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
            "response": full_text,
            "done": False
        }]
    
    # Final response
    final_response = {
        "model": model,
        "created_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "response": "".join(chunks) if not stream else "",  # Empty if we've been streaming
        "done": True,
        "total_duration": 1254857123,
        "load_duration": 548923412,
        "prompt_eval_count": len(prompt),
        "prompt_eval_duration": 321456978,
        "eval_count": 150,
        "eval_duration": 384476733
    }
    response_parts.append(final_response)
    
    return response_parts, 200

# Mock response for chat endpoint
def generate_chat_response(model, messages, stream=True):
    """Generate a mock chat response similar to Ollama's /api/chat endpoint"""
    model_info = next((m for m in available_models if m["name"] == model), None)
    if not model_info:
        return {"error": f"Model '{model}' not found"}, 404
    
    # Extract the last user message for context
    last_message = next((m for m in reversed(messages) if m.get("role") == "user"), None)
    last_content = last_message.get("content", "") if last_message else "No user message found"
    
    response_parts = []
    
    # Initial response
    response_parts.append({
        "model": model,
        "created_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "message": {"role": "assistant", "content": ""},
        "done": False
    })
    
    # Generate mock responses (5 chunks)
    chunks = [
        "Hello! ",
        "I'm a simulated Ollama response. ",
        "This is mocking the chat API. ",
        "Your input was processed successfully. ",
        f"You asked about: '{last_content[:30]}...'"
    ]
    
    # Add content chunks (only if streaming)
    if stream:
        for chunk in chunks:
            response_parts.append({
                "model": model,
                "created_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
                "message": {"role": "assistant", "content": chunk},
                "done": False
            })
    
    # Final response
    final_content = "".join(chunks) if not stream else ""
    final_response = {
        "model": model,
        "created_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "message": {"role": "assistant", "content": final_content},
        "done": True,
        "total_duration": 1254857123,
        "load_duration": 548923412,
        "prompt_eval_count": sum(len(m.get("content", "")) for m in messages),
        "prompt_eval_duration": 321456978,
        "eval_count": 150,
        "eval_duration": 384476733
    }
    response_parts.append(final_response)
    
    return response_parts, 200

# Mock embeddings response
def generate_embeddings(model, prompt):
    """Generate mock embeddings similar to Ollama's /api/embeddings endpoint"""
    # Generate a deterministic but seemingly random list of 768 values
    embedding = [(((i * 37 + ord(c)) % 200) - 100) / 100 for i, c in enumerate(prompt[:768])]
    # Pad to 768 if needed
    embedding = embedding + [0] * (768 - len(embedding))
    # Trim to 768
    embedding = embedding[:768]
    
    return {
        "embedding": embedding
    }, 200

class OllamaHTTPRequestHandler(BaseHTTPRequestHandler):
    def _send_response(self, response_data, status_code=200):
        """Helper method to send JSON response"""
        self.send_response(status_code)
        self.send_header('Content-Type', 'application/json')
        self.end_headers()
        self.wfile.write(json.dumps(response_data).encode("utf-8"))
    
    def _send_streaming_response(self, response_parts, status_code=200):
        """Helper method to send streaming JSON responses"""
        self.send_response(status_code)
        self.send_header('Content-Type', 'application/json')
        self.end_headers()
        
        for part in response_parts:
            self.wfile.write(json.dumps(part).encode("utf-8") + b"\n")
            time.sleep(0.2)  # Simulate some delay between chunks
    
    def _read_request_body(self):
        """Read and parse JSON request body"""
        content_length = int(self.headers.get('Content-Length', 0))
        post_data = self.rfile.read(content_length)
        
        try:
            return json.loads(post_data)
        except json.JSONDecodeError:
            return None
    
    def do_GET(self):
        """Handle GET requests"""
        # Handle /api/tags endpoint
        if self.path == "/api/tags":
            response = {"models": available_models}
            self._send_response(response)
            return
            
        # Handle unknown GET endpoints
        self._send_response({"error": "Not found"}, 404)
    
    def do_POST(self):
        """Handle POST requests"""
        json_data = self._read_request_body()
        
        if json_data is None:
            self._send_response({"error": "Invalid JSON"}, 400)
            return
        
        # Debug: print the received request
        print(f"Received request to {self.path}:")
        print(json.dumps(json_data, indent=2))
        
        # Handle /api/generate endpoint
        if self.path == "/api/generate":
            model = json_data.get("model")
            prompt = json_data.get("prompt", "")
            stream = json_data.get("stream", True)
            
            if not model:
                self._send_response({"error": "Model parameter is required"}, 400)
                return
            
            response_parts, status_code = generate_streaming_response(model, prompt, stream)
            
            if status_code != 200:
                self._send_response(response_parts, status_code)
                return
                
            if stream:
                self._send_streaming_response(response_parts)
            else:
                # For non-streaming, just send the last (complete) response
                self._send_response(response_parts[-1])
            return
        
        # Handle /api/chat endpoint
        elif self.path == "/api/chat":
            model = json_data.get("model")
            messages = json_data.get("messages", [])
            stream = json_data.get("stream", True)
            
            if not model:
                self._send_response({"error": "Model parameter is required"}, 400)
                return
                
            if not messages:
                self._send_response({"error": "Messages parameter is required"}, 400)
                return
            
            response_parts, status_code = generate_chat_response(model, messages, stream)
            
            if status_code != 200:
                self._send_response(response_parts, status_code)
                return
                
            if stream:
                self._send_streaming_response(response_parts)
            else:
                # For non-streaming, just send the last (complete) response
                self._send_response(response_parts[-1])
            return
        
        # Handle /api/embeddings endpoint
        elif self.path == "/api/embeddings":
            model = json_data.get("model")
            prompt = json_data.get("prompt", "")
            
            if not model:
                self._send_response({"error": "Model parameter is required"}, 400)
                return
                
            response, status_code = generate_embeddings(model, prompt)
            self._send_response(response, status_code)
            return
            
        # Handle unknown POST endpoints
        self._send_response({"error": "Not found"}, 404)

if __name__ == "__main__":
    server_address = ('', 11434)  # Same port as Ollama
    httpd = HTTPServer(server_address, OllamaHTTPRequestHandler)
    print(f"Starting Ollama mock server on port {server_address[1]}...")
    print("Available endpoints:")
    print("  GET  /api/tags         - List available models")
    print("  POST /api/generate     - Generate text completions")
    print("  POST /api/chat         - Chat completions")
    print("  POST /api/embeddings   - Generate embeddings")
    httpd.serve_forever()
