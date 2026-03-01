#!/usr/bin/env bash
#
# Copy S3D files from an EverQuest installation into assets/
#
# Usage:
#   ./scripts/copy-assets.sh /path/to/everquest
#   ./scripts/copy-assets.sh /path/to/everquest gfaydark  # specific zone only
#
# Sources:
#   - Steam EQ (free): ~/Library/Application Support/Steam/steamapps/common/EverQuest
#
set -euo pipefail

EQ_DIR="${1:?Usage: $0 <eq-directory> [zone-name]}"
ZONE="${2:-}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ASSETS_DIR="$SCRIPT_DIR/../assets"

if [ ! -d "$EQ_DIR" ]; then
    echo "Error: Directory not found: $EQ_DIR"
    exit 1
fi

mkdir -p "$ASSETS_DIR"

# Good starter zones (small to medium, classic EQ)
STARTER_ZONES=(
    arena        # tiny PvP arena, great for testing
    tutorial     # small tutorial zone
    gfaydark     # classic zone, moderate size
    commons      # East Commonlands
    freportn     # North Freeport
)

if [ -n "$ZONE" ]; then
    # Copy specific zone
    pattern="$EQ_DIR/${ZONE}*.s3d"
    count=0
    for f in $pattern; do
        [ -f "$f" ] || continue
        cp -v "$f" "$ASSETS_DIR/"
        ((count++))
    done
    if [ "$count" -eq 0 ]; then
        echo "No S3D files found matching: $pattern"
        echo "Available S3D files:"
        ls "$EQ_DIR"/*.s3d 2>/dev/null | head -20 | xargs -I{} basename {}
        exit 1
    fi
else
    # Copy starter zones
    echo "Copying starter zone S3D files..."
    for zone in "${STARTER_ZONES[@]}"; do
        for f in "$EQ_DIR"/${zone}*.s3d; do
            [ -f "$f" ] || continue
            cp -v "$f" "$ASSETS_DIR/"
        done
    done

    # Also grab global assets useful for testing
    for f in gequip.s3d global_chr.s3d sky.s3d; do
        if [ -f "$EQ_DIR/$f" ]; then
            cp -v "$EQ_DIR/$f" "$ASSETS_DIR/"
        fi
    done
fi

echo ""
echo "Assets copied to: $ASSETS_DIR"
ls -lh "$ASSETS_DIR"/*.s3d 2>/dev/null || echo "(no S3D files found)"
