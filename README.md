# open-norrath
Experimental Scala based Everquest client designed for EQEmu

Open-source EverQuest client written in Scala, targeting Project 1999 (P1999) compatibility. Licensed under GPL v3.

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

## Architecture / Milestones
1. **Protocol library** — connect, authenticate, enter world, receive entity updates (EQ Titanium-era protocol)
2. **Asset parser** — parse S3D archives and WLD (world) format files into renderable geometry
3. **Zone renderer** — render static zones via LWJGL (BSP/region visibility, baked lightmaps)
4. **Entity system** — positioning, movement, skeletal animation
5. **UI layer** — hotbars, chat, inventory via imgui-java
6. **Everything else** — spells, effects, pathing, etc.

## Key Reference Projects

### EQEmu Server (C++, GPL v3) — Protocol & Game Logic Reference
- **Main repo:** https://github.com/EQEmu/EQEmu
- **Docs:** https://github.com/EQEmu/eqemu-docs-v2 / https://docs.eqemu.dev
- P1999 runs a modified EQEmu fork; packet structures are largely the same
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

To develop and test client networking, run a local EQEmu/Quarm server via Docker.

### Prerequisites
- [Docker](https://docs.docker.com/engine/install/) + Docker Compose

### Setup

```bash
# Clone the Docker environment (includes server, quests, and maps as submodules)
git clone --recurse-submodules https://github.com/nickgal/EQMacDocker
cd EQMacDocker

# Create .env from the example
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

## Settings

`settings.yml` in the project root controls runtime configuration:

```yaml
use_eqg: false
debug:
  animation_model: gor
login:
  host: 127.0.0.1
  port: 6000
```

| Key | Description |
|-----|-------------|
| `use_eqg` | Use EQG companion files for emitters instead of deriving from S3D |
| `debug.animation_model` | Character model to showcase all animations (e.g. `gor`, `dra`, `btm`, `efr`, `lim`, `tig`). Empty to disable. |
| `login.host` | Login server hostname (default `127.0.0.1`) |
| `login.port` | Login server UDP port (default `6000`) |

Zone path is passed as a CLI argument: `sbt "run assets/arena.s3d"` (defaults to `assets/arena.s3d`).

## Debug Tools

Standalone debug tools for inspecting EQ file formats. Run via sbt:

```bash
sbt "runMain opennorrath.tools.ZoneDebug [path.s3d]"
```

All tools default to `assets/arena*` if no argument is given.

| Tool | Target File | What It Dumps |
|------|-------------|---------------|
| `ZoneDebug` | zone `.s3d` | S3D entries, WLD fragment stats, object placements |
| `ObjectDebug` | `_obj.s3d` | Actors, meshes with bounding boxes, materials, textures |
| `CharDebug` | `_chr.s3d` | Actors, skeleton bones, meshes, animation clips |
| `LightDebug` | `.txt` | Line lights with coordinates/RGB, portals |
| `EmitterDebug` | `_EnvironmentEmitters.txt` | Particle emitter names, IDs, positions |

## Key Technical Challenges
- **S3D/WLD format:** Custom archive + world format with BSP trees, region-based visibility, baked lighting. WLD is notoriously underdocumented. LanternExtractor (C#) and zone-utilities (C++) are the best existing parsers.
- **EQ protocol:** Partially documented via EQEmu. Will need Wireshark for gaps. P1999 uses Titanium-era opcodes.
- **Skeletal animation:** Custom bone/attachment system, partially reverse-engineered.
- **Rendering model:** EQ uses BSP/region visibility which doesn't map to standard engine pipelines — reason we chose raw LWJGL over LibGDX.

## Notes
- No existing JVM-based implementations of S3D/WLD parsing — we're porting from C++/C# references.
- LanternEQ is the most actively maintained and complete asset extraction project.
- EQEmu server source is the definitive protocol reference even though it's a server, not client.
