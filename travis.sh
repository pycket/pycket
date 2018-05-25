#/bin/sh
#

# utah or northwestern for prerelease, racket for stable
#DLHOST=utah
DLHOST=northwestern
RACKET_VERSION=current

COVERAGE_TESTSUITE='test'

#

set -e

_help() {
  cat <<EOF
$0 <command>

command is one of

  prepare       Do anything necessary to allow installation
  install       Install direct prerequisites
  test <what>   Test (may include building, testing, coverage)
        tests         Run pytest tests
        coverage      Run pytest coverage report and upload
        translate     Translate pycket with jit
        translate_nojit_and_racket_tests
                     Translate pycket without jit and run racket test


EOF
}


_time_gnu() {
  export TIME="%e"
  (/usr/bin/time "$@" 3>&2 2>&1 1>&3) 2>/dev/null
}

_time_bsd() {
  (/usr/bin/time -p "$@" 3>&2 2>&1 1>&3) 2>/dev/null | \
      grep '^real' | \
      awk '{ print $2; }'
}

_time_builtin() {
  #
  # so there's no /usr/bin/time.
  # lets hope we have a bash/zsh/csh which has a time builtin thats knows -p
  #
  (time -p "$@" >/dev/null) 2>&1 | \
      grep '^real' | \
      awk '{ print $2; }'
}

export LANG=C LC_ALL=C

# config
if type timeout >/dev/null 2>/dev/null; then
  TIMEOUT=timeout
else
  # hope..
  TIMEOUT=gtimeout
fi

if [ ! -e /usr/bin/time ]; then # oh travis...
  TIME_IT=_time_builtin
elif /usr/bin/time --version 2>/dev/null >/dev/null; then
  TIME_IT=_time_gnu
else
  TIME_IT=_time_bsd
fi

GREEN=$(tput setaf 2)
NO_ATTRIB=$(tput sgr0)
RIGHT_SIDE=$(tput cols)

print_console() {
    printf '%s%*s%s\n' "$GREEN" $RIGHT_SIDE "[$1]" "$NO_ATTRIB"
}

############### test targets ################################
do_tests() {

    ./pypy-c ../pypy/pytest.py pycket --ignore=pycket/old-test
}

do_test_expander() {
    ./pypy-c ../pypy/pytest.py pycket --use-expander --ignore=pycket/old-test
}

do_coverage() {
  set +e
  # PyPy's pytest modifications clash with recent pytest-cov/coverage releases
  # So remove them on the CI.
  rm -rf ../pypy/*pytest*
  py.test --assert=plain -n 3 -k "$COVERAGE_TESTSUITE" --cov . --cov-report=term pycket
  codecov -X gcov search
  set -e
  echo '>> Testing whether coverage is over 80%'
  coverage report -i --fail-under=80 --omit='pycket/test/*','*__init__*'
}


do_translate() {
    print_console do_translate
    ./pypy-c ../pypy/rpython/bin/rpython --batch -Ojit --translation-jit_opencoder_model=big targetpycket.py
   #do_performance_smoke
}

# do_performance_smoke() {
#   _smoke() {
#     RATIO=$1
#     shift
#     echo "> $1"
#     raco make $1 >/dev/null
#     RACKET_TIME=$($TIME_IT racket "$@")
#     echo "    Racket took $RACKET_TIME"
#     TARGET_TIME=$(awk "BEGIN { print ${RATIO} * ${RACKET_TIME}; }" )
#     KILLME_TIME=$(awk "BEGIN { print ${TARGET_TIME} * 5; }")
#     racket -l pycket/expand $1 2>/dev/null >/dev/null
#     # $TIMEOUT -k$KILLME_TIME $KILLME_TIME ./pycket-c "$@"
#     PYCKET_TIME=$($TIME_IT ./pycket-c "$@")
#     echo "    Pycket took $PYCKET_TIME (should be < $TARGET_TIME)"
#     [ "OK" = "$(awk "BEGIN { print ($PYCKET_TIME < $TARGET_TIME ? \"OK\" : \"NO\"); }")" ]
#   }
#   echo ; echo ">> Performance smoke test" ; echo
#   echo ">>> Preparing racket files to not skew test"
#   expand_rkt yes
#   echo ">>> Smoke"
#   _smoke 1.5 pycket/test/fannkuch-redux.rkt 10
#   _smoke 0.7 pycket/test/triangle.rkt
#   _smoke 1.8 pycket/test/earley.rkt
#   _smoke 2.0 pycket/test/nucleic2.rkt
#   _smoke 2.5 pycket/test/nqueens.rkt
#   _smoke 2.5 pycket/test/treerec.rkt
#   _smoke 1.5 pycket/test/hashtable-benchmark.rkt
#   echo ; echo ">> Smoke cleared" ; echo
# }

do_translate_nojit_and_racket_tests() {
  print_console do_translate_nojit_and_racket_tests
  ./pypy-c ../pypy/rpython/bin/rpython --batch targetpycket.py
}

############################################################

install_deps() {
  print_console install_deps
  pip install -I pytest-xdist || pip install -I --user pytest-xdist
  if [ $TEST_TYPE = 'coverage' ]; then
    pip install -I codecov pytest-cov || pip install -I codecov pytest-cov
  fi
}

_activate_pypyenv() {
  if [ -f ~/virtualenv/pypy/bin/activate ]; then
    #deactivate 2>&1 >/dev/null || true
    source ~/virtualenv/pypy/bin/activate
  fi
}

install_pypy() {
  PYPY_V=pypy2-v6.0.0-linux64
  PYPY_PAK=$PYPY_V.tar.bz2
  PYPY_URL=https://bitbucket.org/pypy/pypy/downloads/$PYPY_PAK

  print_console "Acquiring pypy binary : "
  wget $PYPY_URL
  tar xjf $PYPY_PAK
  ln -s $PYPY_V/bin/pypy pypy-c

  #pip install -I --upgrade virtualenv
  #virtualenv --no-wheel --no-setuptools --no-pip -p pypy-c/bin/pypy ~/virtualenv/pypy
  # fix virtualenv...
  # rm ~/virtualenv/pypy/bin/libpypy-c.so
  # cp pypy-c/bin/libpypy-c.so ~/virtualenv/pypy/bin/libpypy-c.so
  # _activate_pypyenv
}

fetch_racket() {
  print_console "Fetching the latest Racket"
  git clone https://github.com/racket/racket.git
}

fetch_pypy() {
  print_console "fetch_pypy"
  ###
  #  Prepare pypy
  ###
  wget https://bitbucket.org/pypy/pypy/get/default.tar.bz2 -O `pwd`/../pypy.tar.bz2 || \
      wget https://bitbucket.org/pypy/pypy/get/default.tar.bz2 -O `pwd`/../pypy.tar.bz2
  tar -xf `pwd`/../pypy.tar.bz2 -C `pwd`/../
  mv ../pypy-pypy* ../pypy
}

prepare_racket() {
  ## SET UP THE ENV VARS TO ACCESS RACKET LIBS
  print_console "preparing racket"
}

expand_rkt() {
  if [ $# -gt 0 ]; then
    ALL=$1
    shift
  else
    ALL="no"
  fi

  BASE_MODULES=$(racket -e '(displayln (path->string (path-only (collection-file-path "base.rkt" "racket"))))')
  if [ $ALL != "no" ]; then
      SYNTAX_MODULES=$(racket -e '(displayln (path->string (path-only (collection-file-path "wrap-modbeg.rkt" "syntax"))))')
  fi

  find $BASE_MODULES $SYNTAX_MODULES -type f -name \*.rkt -print0 | \
      while IFS= read -r -d '' F; do
        if [ ! -f "$F.json" ]; then
            printf "%s" .
            racket -l pycket/expand $F 2>/dev/null >/dev/null || true
        fi
      done
}

############################################################


if [ $# -lt 1 ]; then
    print_console "Missing command"
    _help
    exit 1
fi

COMMAND="$1"
shift

_activate_pypyenv

case "$COMMAND" in
  prepare)
    print_console "Preparing dependencies : "
    install_pypy
    #fetch_racket ####### FIXME
    install_deps
    ;;
  install)
    print_console "Fetching pypy and racket : "
    fetch_pypy
    #prepare_racket ###### FIXME
    ;;
  test)
    export PYTHONPATH=$PYTHONPATH:../pypy:pycket
    cp ../pypy/pytest.ini . 2>&1 >/dev/null || true
    if [ -z "$1" ]; then
        print_console "Please tell what to test, see .travis.yml"
        _help
        exit 1
    else
      TEST_TYPE="$1"
      shift
    fi
    print_console "Running $TEST_TYPE"
    do_$TEST_TYPE
    ;;
  *)
    _help
    ;;
esac
