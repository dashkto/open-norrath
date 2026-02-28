## First Steps

- Immediately read the README.md at the root of this project to understand the codebase.

## Reference Projects

- **EQEmu**: A downloaded copy of the EQEmu server source should exist in an adjacent project directory (sibling to this repo). Use it as reference for quest scripts, spawn data, zone mechanics, and server-side logic.
- **EQPcDocker**: A Docker-based EQEmu server setup for the Titanium client, located in the adjacent `EQPcDocker` directory. Repo: https://github.com/dashkto/EQPcDocker

## Asset Paths

- All original EverQuest client assets (models, textures, zones, sounds, etc.) are located at `/assets/EverQuest`.  These are expected to be the Titanium-based client files, which is a PC version, not Mac.

## Logging

- All stdout is written to log files in `/logs`

## Code Style

- Use liberal code comments, especially for surprising behavior, hard-won lessons, and non-obvious gotchas (e.g., "the server never sends X for NPCs"). Don't shy away from explaining *why* something is the way it is.
- There is no linter.  If code changes it is due to another author.
- 4 spaces for indentation, no tabs. Line length should be <= 120 characters, but this is not strictly enforced.
- Use descriptive variable and method names. Avoid abbreviations unless they are widely understood.
- Proper class decomosition is encouraged. If a class exceeds ~200 lines, consider breaking it up into smaller, more focused classes.