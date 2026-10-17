#!/usr/bin/env bash
# Replace sbt's runner scripts (the base of sbta and sbta.bat) with those of an sbt
# release, from its distribution zip: ./update.sh 2.0.10
# Then set SbtLaunchVersion in build.sbt to the same version and run `sbta/sbtaScripts`,
# which fails if one of sbta's edits no longer applies.
set -euo pipefail
version="$1"
dir="$(cd "$(dirname "$0")" && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
curl -fsSL -o "$tmp/sbt.zip" "https://github.com/sbt/sbt/releases/download/v${version}/sbt-${version}.zip"
unzip -q "$tmp/sbt.zip" -d "$tmp"
cp "$tmp/sbt/bin/sbt" "$tmp/sbt/bin/sbt.bat" "$dir/"
echo "sbt runner scripts updated to $version"
