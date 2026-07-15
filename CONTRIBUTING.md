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

- Hand-crafted guides, recipes, and FAQ: [`website/content/`](website/content/)
- Preview locally: `dotnet run --project website` → http://localhost:8080
- API reference is generated with fsdocs:

```bash
./scripts/generate-api-docs.sh
# or
dotnet run --project ./build/build.fsproj -- -t Docs
```

Output is written to `website/content/reference/` (gitignored; CI uploads it as an artifact).

The old Jekyll site under `docs/` is not the live documentation — use it only as historical reference when porting content.

## Pull requests

- Prefer focused PRs (one concern each).
- Include tests for library behavior changes.
- For docs/site changes, note how you previewed them.
