# Contributing to Suave

Thanks for helping improve Suave.

## Development setup

1. Install the [.NET SDK](https://dotnet.microsoft.com/download) matching [`global.json`](global.json).
2. Restore tools and packages:

```bash
dotnet tool restore
dotnet paket restore
```

3. Build: `dotnet build Suave.sln`
4. Tests: `dotnet run -c Release --framework net10.0 --project src/Suave.Tests -- --summary --sequenced`

Coding style: two-space indentation (see [README](README.md) and [`.editorconfig`](.editorconfig)).

## Website and documentation

- Hand-crafted guides, recipes, and FAQ: [`website/content/docs/`](website/content/docs/)
- Preview locally: `dotnet run --project website` → http://localhost:8080
- API reference is generated with fsdocs:

```bash
./scripts/generate-api-docs.sh
# or
dotnet run --project ./build/build.fsproj -- -t Docs
```

The fsdocs input lives in [`docs-api/`](docs-api/) and output is written to `website/content/reference/` (gitignored; CI uploads it as an artifact).

## Releasing

`.semver` holds the version. To cut a release, bump it, add a matching
`## New in vX.Y.Z` section to [`RELEASE_NOTES.md`](RELEASE_NOTES.md), commit,
and push a `vX.Y.Z` tag:

```bash
./build.sh -t Tag
```

The [`Release` workflow](.github/workflows/release.yml) verifies that the tag,
`.semver` and `RELEASE_NOTES.md` agree, then builds, tests, packs, publishes to
nuget.org (Trusted Publishing — no API key secret) and creates the GitHub
Release.

## Pull requests

- Prefer focused PRs (one concern each).
- Include tests for library behavior changes.
- For docs/site changes, note how you previewed them.
