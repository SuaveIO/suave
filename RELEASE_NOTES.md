#### 3.0.0 — Suave v3
* Reworked Suave repository to use FAKE from Albacore.
* Deprecated OpenSSL TLS since TLS has moved on since this was created..
* Deprecated LivUV due to lack of usage/stability.
* Deprecated Razor view engine, see src/Suave.Razor/README.md if you want to use it.
* Removed deprecated functions.
* Transformed Suave to using a binary parser with [ParsecClone][parsecclone]
* Transformed Suave to using Hopac for concurrency
* Upgraded [Logary Library][logary-lib] to v5
* Upgraded [YoLo library][yolo] to latest
* Merged HTTP.fs into this repository

 [parsecclone]: https://github.com/devshorts/ParsecClone
 [logary-lib]: https://github.com/logary/logary#using-logary-in-a-library
 [yolo]: https://github.com/haf/YoLo

