Backend
=======

    stack build managemytime

to run the server, just invoke the executable (it's inside .stack-work) or

    stack exec managemytime-exe

it ships with a builtin self-signed certificate for localhost
to run the integration tests, you'll want unencrypted http, which you can select
with the "test" argument

    stack exec managemytime-exe test
    stack test managemytime:functional-tests

Don't store any important data in the sqlite.db created by default in the project directory
since it will be erased by the testrunner

To create a "production" build (with a proper tls certificate, encrypted inside `tls.yml`)

    ansible-playbook --ask-vault-pass build.yml

Frontend
========

Install the flow type checker:

    opam install flowtype

Install bower and jstransform:

    npm install -g bower jstransform

Typecheck javascript:

    flow check frontend/src/

Parse js and strip signatures:

    jstransform --strip-types --es6module --watch frontend/src frontend/scripts

Install the frontend dependencies (you'll have to symlink them manually):

    bower install
