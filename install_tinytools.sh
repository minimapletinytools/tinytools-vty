#!/bin/bash

# convenience script to downloand and install the current version of tinytools-vty binary

# Define the release version
RELEASE_VERSION="v0.1.0.7"

# Determine the operating system
OS=$(uname -s)

# Determine the OS architecture
ARCH=$(uname -m)
case "$ARCH" in
  x86_64)
    # 64-bit architecture
    ;;
  *)
    echo "Unsupported architecture: $ARCH"
    exit 1
    ;;
esac

# Define the base URL
BASE_URL="https://github.com/minimapletinytools/tinytools-vty/releases/download/$RELEASE_VERSION"

# Set the appropriate binary file based on the OS and architecture
case "$OS" in
  Darwin*)
    # macOS
    ZIP_URL="$BASE_URL/tinytools-x86_64-osx.zip"
    ;;
  Linux*)
    # Linux
    ZIP_URL="$BASE_URL/tinytools-x86_64-linux.zip"
    ;;
  *)
    echo "Unsupported operating system: $OS"
    exit 1
    ;;
esac

# Prompt the user for the installation directory
read -p "Enter the installation directory (default: /usr/local/bin): " INSTALL_DIR
INSTALL_DIR=${INSTALL_DIR:-/usr/local/bin}

# Create a temporary directory to download and extract files
TEMP_DIR=$(mktemp -d)

# Function to cleanup temporary directory
cleanup() {
  rm -rf "$TEMP_DIR"
}

# Trap the script exit and call the cleanup function
trap cleanup EXIT

# Download and extract the zip file
echo "Downloading and extracting tinytools..."
curl -L "$ZIP_URL" -o "$TEMP_DIR/tinytools.zip"
unzip "$TEMP_DIR/tinytools.zip" -d "$TEMP_DIR"

# Move the binary to the installation directory
echo "Installing tinytools to $INSTALL_DIR..."
if mv "$TEMP_DIR/tinytools" "$INSTALL_DIR/"; then
  # Set execute permissions
  chmod +x "$INSTALL_DIR/tinytools"
  echo "tinytools $RELEASE_VERSION has been successfully installed to $INSTALL_DIR"
else
  echo "Error: Failed to move tinytools to $INSTALL_DIR. Installation failed."
  exit 1
fi
