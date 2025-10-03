#!/bin/bash

# Script to update copilot-instructions.md from flipU repository
# Usage: ./update-copilot-instructions.sh

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if we're in a git repository
if [ ! -d ".git" ]; then
    print_error "This script must be run from the root of a git repository"
    exit 1
fi

# Create .github directory if it doesn't exist
if [ ! -d ".github" ]; then
    print_status "Creating .github directory..."
    mkdir -p .github
fi

# Try multiple possible paths for the copilot instructions
POSSIBLE_PATHS=(
    ".github/copilot-instructions.md"
    "copilot-instructions.md"
    ".github/copilot_instructions.md"
    "docs/copilot-instructions.md"
)

TEMP_FILE="/tmp/copilot-instructions-temp.md"
DESIRED_FILE=".github/copilot-instructions.md"

print_status "Searching for copilot-instructions.md in flipU repository..."

DOWNLOADED=false
for path in "${POSSIBLE_PATHS[@]}"; do
    print_status "Trying path: $path"

    HTTP_CODE=$(curl -s -w "%{http_code}" \
                     -H "Accept: application/vnd.github.v3.raw" \
                     -o "/tmp/copilot-instructions-temp.md" \
                     "https://api.github.com/repos/Displayr/flipU/contents/$path" 2>/dev/null || echo "000")

    if [ "$HTTP_CODE" = "200" ]; then
        # Check if we got actual content (not an error response)
        if [ -f "/tmp/copilot-instructions-temp.md" ] && [ -s "/tmp/copilot-instructions-temp.md" ]; then
            # Verify it's not a JSON error response
            if ! grep -q '"message".*"Not Found"' "/tmp/copilot-instructions-temp.md" 2>/dev/null; then
                print_success "Successfully downloaded from: $path"
                mv $TEMP_FILE $DESIRED_FILE
                DOWNLOADED=true
                SOURCE_PATH=$path
                break
            fi
        fi
    elif [ "$HTTP_CODE" = "404" ]; then
        print_status "File not found at: $path"
    else
        print_warning "HTTP $HTTP_CODE when accessing: $path"
    fi
done

# Clean up temp file
rm -f $TEMP_FILE

if [ "$DOWNLOADED" = false ]; then
    print_error "Could not find copilot-instructions.md in flipU repository"
    print_error "Checked paths: ${POSSIBLE_PATHS[*]}"

    # Try to list available files for debugging
    print_status "Attempting to list available files in flipU..."
    curl -s "https://api.github.com/repos/Displayr/flipU/contents" | \
         grep '"name"' | head -10 || print_warning "Could not list repository contents"

    exit 1
fi

# Verify the downloaded file
if [ ! -s $DESIRED_FILE ]; then
    print_error "Downloaded file is empty"
    exit 1
fi

# Check if it looks like a valid copilot instructions file
if ! grep -q -i "copilot\|instruction\|coding\|standard" $DESIRED_FILE; then
    print_warning "Downloaded file may not contain typical copilot instruction content"
    print_warning "Please verify the content manually"
fi

# Show file information
FILE_SIZE=$(wc -c < $DESIRED_FILE)
LINE_COUNT=$(wc -l < $DESIRED_FILE)

print_success "File updated successfully!"
print_status "Source: flipU/$SOURCE_PATH"
print_status "Local: $DESIRED_FILE"
print_status "Size: $FILE_SIZE bytes"
print_status "Lines: $LINE_COUNT"

# Check git status for informational purposes only
if command -v git >/dev/null 2>&1; then
    if git diff --quiet $DESIRED_FILE 2>/dev/null; then
        print_status "No changes detected in ${DESIRED_FILE}"
    else
        print_status "Changes detected in ${DESIRED_FILE}"
        print_status "File updated"
    fi
fi

print_success "Update completed successfully!"
