#!/bin/bash

# Ollama API Demo Script
# This script provides a menu to run various Ollama API commands

# Base URL for Ollama API
BASE_URL="http://localhost:11434/api"

# Text formatting
BOLD=$(tput bold)
NORMAL=$(tput sgr0)
GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 3)
BLUE=$(tput setaf 4)
RED=$(tput setaf 1)

# Function to print section headers
print_header() {
  echo "${BOLD}${BLUE}$1${NORMAL}"
  echo "${BLUE}$(printf '=%.0s' {1..50})${NORMAL}"
}

# Function to print command info
print_command() {
  echo "${BOLD}${GREEN}$1${NORMAL}"
  echo "${YELLOW}$2${NORMAL}"
  echo
}

# Function to print response
print_response() {
  echo "${BOLD}Response:${NORMAL}"
  echo "$1" | jq -C . 2>/dev/null || echo "$1"
  echo
}

# Function to run a curl command and display the response
run_curl() {
  local description=$1
  local command=$2
  
  print_command "$description" "$command"
  
  # Ask for confirmation
  read -p "Run this command? (y/n): " choice
  if [[ "$choice" == "y" || "$choice" == "Y" ]]; then
    echo "Running command..."
    response=$(eval "$command")
    print_response "$response"
  else
    echo "Skipped"
    echo
  fi
}

# Function for Generate a completion
generate_completion() {
  print_header "Generate a Completion"
  
  run_curl "Basic Generation (No Streaming)" \
    "curl $BASE_URL/generate -d '{\"model\": \"llama3.2\", \"prompt\": \"Why is the sky blue?\", \"stream\": false}'"
  
  run_curl "Generate with Suffix" \
    "curl $BASE_URL/generate -d '{\"model\": \"codellama:code\", \"prompt\": \"def compute_gcd(a, b):\", \"suffix\": \"    return result\", \"options\": {\"temperature\": 0}, \"stream\": false}'"
  
  run_curl "Structured Outputs" \
    "curl -X POST $BASE_URL/generate -H \"Content-Type: application/json\" -d '{\"model\": \"llama3.1:8b\", \"prompt\": \"Ollama is 22 years old and is busy saving the world. Respond using JSON\", \"stream\": false, \"format\": {\"type\": \"object\", \"properties\": {\"age\": {\"type\": \"integer\"}, \"available\": {\"type\": \"boolean\"}}, \"required\": [\"age\", \"available\"]}}'"
  
  run_curl "JSON Mode" \
    "curl $BASE_URL/generate -d '{\"model\": \"llama3.2\", \"prompt\": \"What color is the sky at different times of the day? Respond using JSON\", \"format\": \"json\", \"stream\": false}'"
  
  run_curl "Reproducible Outputs" \
    "curl $BASE_URL/generate -d '{\"model\": \"mistral\", \"prompt\": \"Why is the sky blue?\", \"options\": {\"seed\": 123}}'"
  
  run_curl "Load a Model" \
    "curl $BASE_URL/generate -d '{\"model\": \"llama3.2\"}'"
  
  run_curl "Unload a Model" \
    "curl $BASE_URL/generate -d '{\"model\": \"llama3.2\", \"keep_alive\": 0}'"
}

# Function for Generate a chat completion
generate_chat_completion() {
  print_header "Generate a Chat Completion"
  
  run_curl "Chat Request (No Streaming)" \
    "curl $BASE_URL/chat -d '{\"model\": \"llama3.2\", \"messages\": [{\"role\": \"user\", \"content\": \"why is the sky blue?\"}], \"stream\": false}'"
  
  run_curl "Chat Request (With History)" \
    "curl $BASE_URL/chat -d '{\"model\": \"llama3.2\", \"messages\": [{\"role\": \"user\", \"content\": \"why is the sky blue?\"}, {\"role\": \"assistant\", \"content\": \"due to rayleigh scattering.\"}, {\"role\": \"user\", \"content\": \"how is that different than mie scattering?\"}]}'"
  
  run_curl "Chat Request (Structured Outputs)" \
    "curl -X POST $BASE_URL/chat -H \"Content-Type: application/json\" -d '{\"model\": \"llama3.1\", \"messages\": [{\"role\": \"user\", \"content\": \"Ollama is 22 years old and busy saving the world. Return a JSON object with the age and availability.\"}], \"stream\": false, \"format\": {\"type\": \"object\", \"properties\": {\"age\": {\"type\": \"integer\"}, \"available\": {\"type\": \"boolean\"}}, \"required\": [\"age\", \"available\"]}, \"options\": {\"temperature\": 0}}'"
  
  run_curl "Chat Request (with Tools)" \
    "curl $BASE_URL/chat -d '{\"model\": \"llama3.2\", \"messages\": [{\"role\": \"user\", \"content\": \"What is the weather today in Paris?\"}], \"stream\": false, \"tools\": [{\"type\": \"function\", \"function\": {\"name\": \"get_current_weather\", \"description\": \"Get the current weather for a location\", \"parameters\": {\"type\": \"object\", \"properties\": {\"location\": {\"type\": \"string\", \"description\": \"The location to get the weather for, e.g. San Francisco, CA\"}, \"format\": {\"type\": \"string\", \"description\": \"The format to return the weather in, e.g. 'celsius' or 'fahrenheit'\", \"enum\": [\"celsius\", \"fahrenheit\"]}}, \"required\": [\"location\", \"format\"]}}}]}'"
  
  run_curl "Load a Model (Chat)" \
    "curl $BASE_URL/chat -d '{\"model\": \"llama3.2\", \"messages\": []}'"
  
  run_curl "Unload a Model (Chat)" \
    "curl $BASE_URL/chat -d '{\"model\": \"llama3.2\", \"messages\": [], \"keep_alive\": 0}'"
}

# Function for model management
model_management() {
  print_header "Model Management"
  
  run_curl "Create a Model" \
    "curl $BASE_URL/create -d '{\"model\": \"mario\", \"from\": \"llama3.2\", \"system\": \"You are Mario from Super Mario Bros.\"}'"
  
  run_curl "List Local Models" \
    "curl $BASE_URL/tags"
  
  run_curl "Show Model Information" \
    "curl $BASE_URL/show -d '{\"model\": \"llama3.2\"}'"
  
  run_curl "Copy a Model" \
    "curl $BASE_URL/copy -d '{\"source\": \"llama3.2\", \"destination\": \"llama3-backup\"}'"
  
  run_curl "Delete a Model" \
    "curl -X DELETE $BASE_URL/delete -d '{\"model\": \"llama3-backup\"}'"
  
  run_curl "Pull a Model" \
    "curl $BASE_URL/pull -d '{\"model\": \"llama3.2\", \"stream\": false}'"
}

# Function for generating embeddings
generate_embeddings() {
  print_header "Generating Embeddings"
  
  run_curl "Generate Embedding (Single Input)" \
    "curl $BASE_URL/embed -d '{\"model\": \"all-minilm\", \"input\": \"Why is the sky blue?\"}'"
  
  run_curl "Generate Embedding (Multiple Inputs)" \
    "curl $BASE_URL/embed -d '{\"model\": \"all-minilm\", \"input\": [\"Why is the sky blue?\", \"Why is the grass green?\"]}'"
  
  run_curl "Generate Embedding (Legacy Endpoint)" \
    "curl $BASE_URL/embeddings -d '{\"model\": \"all-minilm\", \"prompt\": \"Here is an article about llamas...\"}'"
}

# Function for utility endpoints
utility_endpoints() {
  print_header "Utility Endpoints"
  
  run_curl "List Running Models" \
    "curl $BASE_URL/ps"
  
  run_curl "Version" \
    "curl $BASE_URL/version"
}

# Main menu function
main_menu() {
  clear
  print_header "Ollama API Demo Script"
  echo "This script allows you to run various Ollama API commands."
  echo
  echo "Select a category:"
  echo "1) Generate a Completion"
  echo "2) Generate a Chat Completion"
  echo "3) Model Management"
  echo "4) Generate Embeddings"
  echo "5) Utility Endpoints"
  echo "6) Run All Commands"
  echo "0) Exit"
  echo
  read -p "Enter your choice: " choice
  
  case $choice in
    1) generate_completion; press_enter_to_continue; main_menu ;;
    2) generate_chat_completion; press_enter_to_continue; main_menu ;;
    3) model_management; press_enter_to_continue; main_menu ;;
    4) generate_embeddings; press_enter_to_continue; main_menu ;;
    5) utility_endpoints; press_enter_to_continue; main_menu ;;
    6) run_all_commands; press_enter_to_continue; main_menu ;;
    0) echo "Exiting..."; exit 0 ;;
    *) echo "Invalid choice"; press_enter_to_continue; main_menu ;;
  esac
}

# Function to run all commands
run_all_commands() {
  print_header "Running All Commands"
  generate_completion
  generate_chat_completion
  model_management
  generate_embeddings
  utility_endpoints
}

# Function to wait for user input before continuing
press_enter_to_continue() {
  echo
  read -p "Press Enter to continue..."
}

# Check if jq is installed
if ! command -v jq &> /dev/null; then
  echo "${RED}Warning: jq is not installed. JSON responses will not be formatted.${NORMAL}"
  echo "You can install jq with: sudo apt install jq (Ubuntu/Debian) or brew install jq (macOS)"
  echo
  read -p "Press Enter to continue anyway..."
fi

# Start the script
main_menu
