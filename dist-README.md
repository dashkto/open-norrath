# OpenNorrath

An EverQuest client built from scratch.

## Requirements

- **Windows:** Java 21 is bundled — no install needed.
- **macOS / Linux:** Install [Java 21+](https://adoptium.net/) and make sure `java` is on your PATH.

## Setup

1. **Place your EverQuest files.** Copy your EverQuest game folder into the `assets` directory
   so it lives at `assets/EverQuest/`. The app needs the original Titanium-era S3D files
   (models, textures, zones, sounds). The Steam version of EverQuest works.

   Your folder structure should look like:
   ```
   OpenNorrath/
     open-norrath.jar
     settings.yml
     run.bat / run.sh
     assets/
       EverQuest/
         gfaydark.s3d
         arena.s3d
         ...
   ```

2. **Edit `settings.yml`** to configure your server connection. The `active_server` field
   selects which server profile to use. You can add your own under `servers:`.

## Running

**Windows:** Double-click `run.bat`

**macOS / Linux:** Run `./run.sh` from a terminal.

To load a specific zone directly (skipping the login screen), pass the path as an argument:
```
java -jar open-norrath.jar assets/gfaydark.s3d
```

## Files

| File/Folder     | Purpose |
|-----------------|---------|
| `open-norrath.jar` | The application |
| `settings.yml`  | Server connection and rendering settings (edit this) |
| `imgui.ini`     | Window layout — auto-generated on first run. Delete to reset UI layout |
| `logs/`         | Session logs (one per launch) |
| `assets/`       | Place your EverQuest folder here |

## Troubleshooting

- **"No such file" on launch** — Make sure you're running from the OpenNorrath folder,
  not from somewhere else.
- **Black screen / no zones** — Verify your EQ files are at `assets/EverQuest/` and contain `.s3d` files.
- **Java not found** — Install Java 17+ and make sure `java` is on your PATH.
