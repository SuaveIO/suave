---
layout: default
---

More Applicatives and HTTP combinators
---------------------------------

The documentation for applicates is written in
[Http.fsi](https://github.com/SuaveIO/suave/blob/master/src/Suave/Http.fsi) - so
it's recommended you explore the module using the code-completion of your IDE,
or by looking at the linked file.

All-in-all, the Http module consists of these sub-modules:

<dl>
<dt>Codes</dt>
<dd>simply hosts the HttpCode type.</dd>
<dt>Internals</dt>
<dd>constants and version of suave library.</dd>
<dt>Compression</dt>
<dd>Functions for compressing responses.</dd>
<dt>Response</dt>
<dd>response and response_f functions.</dd>
<dt>Writers</dt>
<dd>ways to modify the response.</dd>
<dt>Intermediate - </dt>
<dd>100 and 101 response codes.</dd>
<dt>Successful</dt>
<dd>2xx response codes.</dd>
<dt>Redirection</dt>
<dd>3xx response codes.</dd>
<dt>RequestErrors - </dt>
<dd>4xx response codes.</dd>
<dt>ServerErrors</dt>
<dd>5xx response codes.</dd>
<dt>Applicatives</dt>
<dd>use to filter down the request to something you</dd>
<dt>Files</dt>
<dd>send files to the client</dd>
<dt>Authentication</dt>
<dd>Methods for authenticating http requests</dd>
</dl>

If you can get the FSFormatting project to work, we would appreciate a PR with
generated documentation from the Http module as we've literally transcribed the
RFC that documents all HTTP result codes, into this file.

The numeric/discriminated-union HTTP codes can be found in `Suave.Types.Codes`.