escheme Conformance
===================

## Introduction

escheme is an aspiring R3RS implementation of the algorithmic language scheme, 
supporting approximately 90% of the standard. Further, many additional 
functions are added to support environments, input/output and unix.

Energy has been directed initially at core implementation and robustness, 
not in completing the set of standard functions. But that task is being
addressed with time.

For a complete description of the R3RS see:

   https://practical-scheme.net/wiliki/schemexref.cgi?R3RS

## R3RS Features

R3RS features supported (Y) and not supported (N) by escheme:
```
  Expressions
    quote               Y
    lambda              Y
    if                  Y
    set!                Y
    cond                Y
    case                Y (macro)
    and                 Y
    or                  Y
    let                 Y
    let*                Y (macro)
    letrec              Y
    begin               Y
    do                  Y (macro)
    delay               Y
    quasiquote          Y

  Programe Structure
    define              Y

  Booleans
    not                 Y
    boolean?            Y
    nil                 Y
    t                   Y

  Equivalence Predicates
    eqv?                Y
    eq?                 Y
    equal?              Y

  Pairs and Lists
    pair?               Y
    cons                Y
    car                 Y
    cdr                 Y
    set-car!            Y
    set-cdr!            Y
    null?               Y
    list                Y
    length              Y
    append              Y
    reverse             Y
    list-tail           Y
    list-ref            Y
    last-pair           Y
    memq                Y
    memv                Y
    member              Y
    assq                Y
    assv                Y
    assoc               Y

  Symbols
    symbol?             Y
    string->symbol      Y
    symbol->string      Y

  Numbers
    number?             Y
    complex?                    N
    real?               Y
    rational?                   N
    integer?            Y
    zero?               Y
    positive?           Y
    negative?           Y
    odd?                Y
    even?               Y
    exact?                      N
    inexact?                    N
    =                   Y
    <                   Y
    >                   Y
    <=                  Y
    >=                  Y
    max                 Y
    min                 Y
    +                   Y
    *                   Y
    -                   Y
    /                   Y
    abs                 Y
    quotient            Y
    remainder           Y
    modulo                      N
    numerator                   N
    denominator                 N
    gcd                 Y
    lcm                         N
    floor               Y
    ceiling             Y
    truncate            Y
    round               Y
    rationalize                 N
    exp                 Y
    log                 Y
    sin                 Y
    cos                 Y
    tan                 Y
    asin                Y
    acos                Y
    atan                Y
    sqrt                Y
    expt                Y
    make-rectangular            N
    make-polar                  N

  Characters
    char?               Y
    char=?              Y
    char<?              Y
    char>?              Y
    char>=?             Y
    char<=?             Y
    char-ci=?           Y
    char-ci<?           Y
    char-ci>?           Y
    char-ci<=?          Y
    char-ci>=?          Y
    char-alphabetic?    Y
    char-numeric?       Y
    char-whitespace?    Y
    char-upper-case?    Y
    char-lower-case?    Y
    char->integer       Y
    integer->char       Y
    char-upcase         Y
    char-downcase       Y

  Strings
    string?             Y
    make-string         Y
    string-length       Y
    string-ref          Y
    string-set!         Y
    string=?            Y
    string<?            Y
    string>?            Y
    string<=?           Y
    string>=?           Y
    string-ci=?         Y
    string-ci<?         Y
    string-ci>?         Y
    string-ci<=?        Y
    string-ci>=?        Y
    substring           Y
    string-append       Y
    string->list        Y
    list->string        Y
    string-copy         Y
    string-fill!        Y

  Vector
    vector?             Y
    make-vector         Y
    vector              Y
    vector-length       Y
    vector-ref          Y
    vector-set!         Y
    vector->list        Y
    list->vector        Y
    vector-fill!        Y

  Control Features
    procedure?          Y
    apply               Y
    map                 Y
    for-each            Y
    force               Y
    call/cc             Y
    call-with-input-file        N
    call-with-output-file       N
    input-port?         Y
    output-port?        Y
    current-input-port          N
    current-output-port         N
    with-input-from-file        N
    with-output-to-file         N
    open-input-file     Y
    open-output-file    Y
    close-input-port    Y
    close-output-port   Y
    read                Y
    read-char           Y
    char-ready?                 N
    eof-object?         Y
    write               Y
    display             Y
    newline             Y
    write-char          Y
    load                Y
    transcript-on       Y
    transcript-off      Y

escheme additions

  Special Forms
    access
    cond
    while

  Numbers
    logand
    logior
    logxor
    lognot
    shift-right
    shift-lefth
    shift-right-arithmetic

  Unix/Linux
    system
    getargs
    gettime
    getenv
    setenv
    unsetenv
    chdir
    getcwd
    getenv

  Environments
    %make-environment
    the-environment
    the-global-environment
    procedure-environment
    environment-bindings
    environment-parent

  Ports
    open-input-string
    open-output-string
    get-output-string
    open-append-file
    open-update-file
    close-port
    flush-output

  Symbols
    gensym
    get
    put

  Dictionaries
    make-dict
    dict?
    dict-items
    has-key?
    dict-ref
    dict-set!
    dict-rem!
    dict-empty!

  Associative Environments
    %make-assoc-env
    assoc-env?
    %assoc-env-has?
    %assoc-env-ref
    %assoc-env-set!

  Strings
    string-null?
    string-find
    string-dup
    string-trim
    string-trim-left
    string-trim-right
    string-downcase!
    string-upcase!
    string-pad-left
    string-pad-right

  Sockets
    socket-read
    socket-write
    socket-recvfrom
    socket-recv
    socket-sendto
    socket-create-tcp
    socket-create-udp
    socket-bind
    socket-bind-address
    socket-create-address
    socket-listen
    socket-accept
    socket-connect
    socket-disconnect
    socket-close
    read-select

  Vectors
    vector-copy!

  Byte Vector
    byte-vector
    make-byte-vector
    byte-vector-ref
    byte-vector-set!
    byte-vector-length

  Promises
    promise?

  Internals
    %closure-code
    %closure-benv
    %closure-vars
    %closure-numv
    %closure-rest
    %make-code
    %get-bcodes
    %get-sexprs
    %car
    %cdr
    gc
    gc-enable
    gc-disable
    %object-address

  Command History (linenoise integration)
    add-history
    show-history
    clear-history
    set-prompt

```

