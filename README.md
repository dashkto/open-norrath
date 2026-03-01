# open-norrath
Open-source EverQuest client written in Scala, targeting Quarm (TAKP) compatibility. Licensed under GPL v3.

## Prerequisites
- JDK 21+
- [sbt](https://www.scala-sbt.org/)
- An EverQuest installation — copy or symlink the entire EverQuest folder into `assets/EverQuest/` (e.g. `assets/EverQuest/arena.s3d`, `assets/EverQuest/gequip.s3d`, etc.)

## Running
```bash
sbt run
```
Opens a 1280x720 window. Press Escape to quit.

## Tech Stack
- **Language:** Scala
- **Graphics:** LWJGL (raw OpenGL bindings, not LibGDX — we want full control of the GL pipeline since EQ's rendering model is too custom for LibGDX's abstractions)
- **Math:** JOML (Java OpenGL Math Library)
- **UI:** imgui-java (Dear ImGui bindings) for EQ-style dockable/resizable windows
- **Windowing/Input/Audio:** GLFW + OpenAL via LWJGL

## Key Reference Projects

### EQEmu Server (C++, GPL v3) — Protocol & Game Logic Reference
- **Main repo:** https://github.com/EQEmu/EQEmu
- **Docs:** https://github.com/EQEmu/eqemu-docs-v2 / https://docs.eqemu.dev
- Contains opcode definitions, packet structures, and game mechanic implementations
- EQExtractor2 lives inside the server repo at `utils/EQExtractor2/` (.NET, packet capture tool)

### EQEmu Zone Utilities (C++, GPL v2) — Zone File Parsing
- **Repo:** https://github.com/EQEmu/zone-utilities
- S3D/WLD/EQG parsing, zone rendering, navmesh generation
- Best C++ reference for the asset pipeline

### EQExtractor (C#) — Standalone Packet Extraction
- **Repo:** https://github.com/EQEmu/EQExtractor

### LanternEQ (C#/.NET + Unity) — Asset Extraction & Zone Rendering
- **Organization:** https://github.com/LanternEQ
- **LanternExtractor:** https://github.com/LanternEQ/LanternExtractor — S3D file extractor, exports to modern engine formats
- **LanternUnityTools:** https://github.com/LanternEQ/LanternUnityTools — Unity zone importer with custom EQ shaders
- **Website:** https://lanterneq.com/
- Excellent reference for S3D/WLD parsing and accurate EQ lighting/rendering

### Project Quarm / EQMacEmu (C++, GPL v2) — Mac-Era EQEmu Fork
- **Repo:** https://github.com/SecretsOTheP/EQMacEmu
- EQEmu fork targeting the Mac/Intel client era
- Project Quarm runs on this codebase

### EQEmu Maps
- **Repo:** https://github.com/EQEmu/maps

## Local EQEmu Server

### EQPcDocker (Titanium / PC Clients)

Docker-based EQEmu server for Titanium and other PC clients, using the mainline EQEmu server and PEQ database.

- **Repo:** https://github.com/dashkto/EQPcDocker

```bash
git clone --recurse-submodules https://github.com/dashkto/EQPcDocker
cd EQPcDocker
cp .env.example .env   # edit SERVER_ADDRESS
docker compose up
```

### EQMacDocker (Quarm / Mac Client)

Docker-based server for the Mac/Quarm client, using the EQMacEmu fork.

- **Repo:** https://github.com/nickgal/EQMacDocker

```bash
git clone --recurse-submodules https://github.com/nickgal/EQMacDocker
cd EQMacDocker
cp .env.example .env
```

Edit `.env` and set:
```
DATABASE_USER=quarm
DATABASE_PASSWORD=quarmpassword
DATABASE_NAME=quarm
SERVER_ADDRESS=127.0.0.1
SERVER_SHORT_NAME=Quarm Docker
SERVER_LONG_NAME=Quarm Docker
```

```bash
# Build and start (first build compiles the C++ server, takes ~10 minutes)
docker compose up
```

### Server Ports

| Service | Port | Description |
|---------|------|-------------|
| Login   | 6000 | Login server — authentication and server list |
| World   | 9000 | World server — character select, zone routing |
| Zones   | 7000-7400 | Zone servers — actual gameplay |
| UCS     | 7778 | Universal chat service |

### Notes
- The Quarm README references unmerged PRs #2 and #3 on the EQMacDocker repo. PR #2 updates env var naming to Quarm conventions (applied above). PR #3 updates the DB dump reference (the repo already has the latest dump).
- Database is MariaDB, auto-initialized from the Quarm SQL dump on first `docker compose up`.
- Server source is the EQMacEmu fork (SecretsOTheP/EQMacEmu) at `Server/` submodule.
- **Rancher Desktop users:** The default SSH port forwarder only forwards TCP. The login server uses UDP, so you must disable it: `rdctl set --experimental.virtual-machine.ssh-port-forwarder=false`. This switches to Lima's native port forwarding which supports UDP. Docker Desktop for Mac may have similar UDP limitations.

### Querying the Database

The EQEmu database is the source of truth for game data — items, spells, NPCs, zones, starting equipment, etc. You can query it directly to understand server behavior or debug client issues.

```bash
# Interactive MySQL shell
docker exec -it eqmacdocker-db-1 mysql -u quarm -pquarmpassword quarm

# One-off queries
docker exec eqmacdocker-db-1 mysql -u quarm -pquarmpassword quarm -e "SELECT * FROM items WHERE id = 9998;"
```

Useful tables:

| Table | Description |
|-------|-------------|
| `items` | All item definitions — name, stats, slots, icon, item class |
| `starting_items` | Items given to new characters (by race/class, `slot=-1` = auto-assign) |
| `npc_types` | NPC definitions — name, race, class, level, stats |
| `spawn2` / `spawnentry` / `spawngroup` | Spawn points and NPC assignments per zone |
| `zone` | Zone metadata — short/long names, safe coordinates, level range |
| `start_zones` | Starting zone assignments by race/class/deity |
| `spells_new` | Spell definitions — name, effects, cast time, mana cost |
| `lootdrop` / `loottable` | Loot tables and drop rates |

Example queries:
```sql
-- What items does a Human Warrior start with?
SELECT si.slot, si.itemid, i.Name
FROM starting_items si
JOIN items i ON si.itemid = i.id
WHERE (si.race = 0 OR si.race = 1) AND (si.class = 0 OR si.class = 1);

-- What NPCs spawn in a zone?
SELECT nt.name, nt.level, nt.race, s2.x, s2.y, s2.z
FROM spawn2 s2
JOIN spawnentry se ON s2.spawngroupID = se.spawngroupID
JOIN npc_types nt ON se.npcID = nt.id
WHERE s2.zone = 'arena';

-- Look up an item by name
SELECT id, Name, ac, hp, mana, damage, delay, slots FROM items WHERE Name LIKE '%Short Sword%';
```

## Public Login Servers

| Server | Host | Port |
|--------|------|------|
| Project Quarm (TAKP-based) | `loginserver.takproject.net` | 6000 |

## Settings

`settings.yml` in the project root controls runtime configuration.

### Server Profiles

Define named server profiles under `servers:` and point `active_server:` at the one you want:

```yaml
active_server: takp

servers:
  local_titanium:
    host: 127.0.0.1
    port: 6000
    world_port: 9000
    mac_mode: false
  takp:
    host: loginserver.takproject.net
    port: 6000
    world_port: 9000
    mac_mode: true
```

| Key | Description |
|-----|-------------|
| `active_server` | Name of the server profile to use |
| `servers.<name>.host` | Login server hostname |
| `servers.<name>.port` | Login server UDP port |
| `servers.<name>.world_port` | World server port (default `9000`) |
| `servers.<name>.mac_mode` | `true` = Mac protocol, `false` = Titanium/PC protocol |
| `use_eqg` | Use EQG companion files for emitters instead of deriving from S3D |
| `debug.animation_model` | Character model to showcase all animations (e.g. `gor`, `dra`, `btm`). Empty to disable. |

Zone path is passed as a CLI argument: `sbt "run assets/arena.s3d"` (defaults to `assets/arena.s3d`).

## UI Window Layout

Window positions and sizes are persisted by Dear ImGui in `imgui.ini` (project root). Panel code sets `defaultX`/`defaultY` with `ImGuiCond.FirstUseEver` as initial fallbacks, but after the first run the ini file takes over. Delete `imgui.ini` to reset all window positions.

## Equipment DDS Textures

Equipment models from `gequip3.s3d` and later (IT10000+) are Luclin-era items that reference
`.dds` textures instead of `.bmp`. The Titanium client assets only include BMP textures inside
the S3D archives — DDS files are not present. These items will render with the checkerboard
fallback texture. Classic items (IT1–IT999 in `gequip.s3d`) all have BMP textures and render
correctly.
