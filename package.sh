#!/bin/bash
set -e
cd "$(dirname "$0")"

APP="OpenNorrath"
JAR_NAME="open-norrath.jar"
VERSION=$(grep 'version :=' build.sbt | sed 's/.*"\(.*\)".*/\1/')
SHA=$(git rev-parse --short HEAD)
DIST="target/dist/$APP"
ZIP_NAME="${APP}-${VERSION}-${SHA}.zip"
JRE_ZIP="target/dist/jre-windows.zip"
JRE_URL="https://api.adoptium.net/v3/binary/latest/21/ga/windows/x64/jre/hotspot/normal/eclipse"

echo "Building fat jar..."
sbt assembly

echo "Creating distribution..."
rm -rf "$DIST" target/dist/$APP-*.zip
mkdir -p "$DIST/assets" "$DIST/logs"

# Jar
cp "target/scala-3.6.3/$JAR_NAME" "$DIST/"

# Config files — ship defaults for a clean first run
cp settings.yml "$DIST/"
# Don't copy imgui.ini — let ImGui generate a fresh one on first launch

# Bundle Windows JRE 21
if [ ! -f "$JRE_ZIP" ]; then
  echo "Downloading Windows JRE 21..."
  curl -L -o "$JRE_ZIP" "$JRE_URL"
fi
echo "Bundling JRE..."
rm -rf "$DIST/jre"
unzip -q "$JRE_ZIP" -d "$DIST/jre"
# Flatten the nested jdk-* directory
mv "$DIST"/jre/jdk-*/* "$DIST/jre/"
rmdir "$DIST"/jre/jdk-*

# Launcher scripts
cat > "$DIST/run.bat" << 'BAT'
@echo off
cd /d "%~dp0"
if exist jre\bin\java.exe (
    jre\bin\java.exe -jar open-norrath.jar %*
) else (
    java -jar open-norrath.jar %*
)
pause
BAT

cat > "$DIST/run.sh" << 'SH'
#!/bin/bash
cd "$(dirname "$0")"
java -XstartOnFirstThread -jar open-norrath.jar "$@"
SH
chmod +x "$DIST/run.sh"

# README
cp dist-README.md "$DIST/README.md"

# Zip it up
echo "Creating zip..."
cd target/dist
zip -qr "$ZIP_NAME" "$APP"
cd ../..

echo ""
echo "Done!"
ls -lh "target/dist/$ZIP_NAME"
