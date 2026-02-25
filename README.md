# open-norrath
Experimental Scala based Everquest client designed for EQEmu

Open-source EverQuest client written in Scala, targeting Project 1999 (P1999) compatibility. Licensed under GPL v3.

## Prerequisites
- JDK 21+
- [sbt](https://www.scala-sbt.org/)

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

### EQEmu Maps
- **Repo:** https://github.com/EQEmu/maps

## Key Technical Challenges
- **S3D/WLD format:** Custom archive + world format with BSP trees, region-based visibility, baked lighting. WLD is notoriously underdocumented. LanternExtractor (C#) and zone-utilities (C++) are the best existing parsers.
- **EQ protocol:** Partially documented via EQEmu. Will need Wireshark for gaps. P1999 uses Titanium-era opcodes.
- **Skeletal animation:** Custom bone/attachment system, partially reverse-engineered.
- **Rendering model:** EQ uses BSP/region visibility which doesn't map to standard engine pipelines — reason we chose raw LWJGL over LibGDX.

## Notes
- No existing JVM-based implementations of S3D/WLD parsing — we're porting from C++/C# references.
- LanternEQ is the most actively maintained and complete asset extraction project.
- EQEmu server source is the definitive protocol reference even though it's a server, not client.
