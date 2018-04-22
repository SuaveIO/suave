# Suave backend

 - Uses Suave (big surprise!)
 - Uses master-branch version of Suave

## Running

    MONO_TLS_PROVIDER=btls STRIPE_PRIVATE_KEY="sk_test_wKhdTXjsPFI1YcISwxzXx3EP" mono docs/server/bin/Debug/server.exe --binding 127.0.0.1 8081 --home /Users/h/dev/suave/suave/docs/_site
    cd docs && bundle exec jekyll build --watch
