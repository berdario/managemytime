Install the flow type checker:

    opam install flowtype

Install bower and jstransform:

    npm install -g bower jstransform

Typecheck javascript:

    flow check frontend/src/

Parse js and strip signatures:

    jstransform --strip-types --es6module --watch frontend/src frontend/scripts
