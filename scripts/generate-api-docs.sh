#!/usr/bin/env bash
# Generate Suave API reference into website/content/reference using fsdocs.
# Absolute project paths are required (Ionide ProjInfo URI resolution).
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

dotnet tool restore

echo "Building libraries (Release)..."
dotnet build -c Release "$ROOT/src/Suave/Suave.fsproj"
dotnet build -c Release "$ROOT/src/Suave.Json/Suave.Json.fsproj"
dotnet build -c Release "$ROOT/src/Suave.DotLiquid/Suave.DotLiquid.fsproj"

OUT_TMP="$ROOT/tmp/fsdocs-out"
DEST="$ROOT/website/content/reference"
rm -rf "$OUT_TMP"
mkdir -p "$OUT_TMP"

echo "Running fsdocs..."
dotnet fsdocs build \
  --input "$ROOT/docs-api" \
  --output "$OUT_TMP" \
  --projects \
    "$ROOT/src/Suave/Suave.fsproj" \
    "$ROOT/src/Suave.Json/Suave.Json.fsproj" \
    "$ROOT/src/Suave.DotLiquid/Suave.DotLiquid.fsproj" \
  --parameters root /reference/ \
  --properties Configuration=Release

echo "Installing into $DEST ..."
rm -rf "$DEST"
mkdir -p "$DEST"
# API HTML lives under output/reference/
cp -R "$OUT_TMP/reference/." "$DEST/"
# Tips / search assets
if [[ -d "$OUT_TMP/content" ]]; then
  cp -R "$OUT_TMP/content" "$DEST/content"
fi

# fsdocs emits links as {root}reference/...; with root=/reference/ that becomes
# /reference/reference/... — rewrite after flattening into website/content/reference.
find "$DEST" -type f \( -name '*.html' -o -name '*.js' -o -name '*.json' \) -print0 \
  | xargs -0 perl -pi -e 's#/reference/reference/#/reference/#g'

# Keep tips.js for member tooltips; drop the stock layout CSS (incompatible with our chrome).
if [[ -d "$DEST/content" ]]; then
  find "$DEST/content" -maxdepth 1 -type f \( -name 'fsdocs-default.css' -o -name 'fsdocs-generated.css' -o -name 'fsdocs-theme*.css' -o -name 'fsdocs-tips.css' \) -delete
fi

# Drop fsdocs extras that must not ship with the site
rm -f "$DEST/Dockerfile" "$DEST/NuGet.config" 2>/dev/null || true
rm -f "$OUT_TMP/Dockerfile" "$OUT_TMP/NuGet.config" 2>/dev/null || true

echo "API docs ready at website/content/reference ($(find "$DEST" -name '*.html' | wc -l | tr -d ' ') HTML files)."
