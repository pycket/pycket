#/bin/sh
set -x
#

# utah or northwestern for prerelease, racket for stable
DLHOST=utah
RACKET_VERSION=current

COVERAGE_TESTSUITE='not (test_larger or test_contract_structs or test_quote_syntax_expansion)'

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

if [ -x pypy-c/bin/pypy ]; then
    virtualenv -p pypy-c/bin/pypy .
fi

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




############### test targets ################################
do_tests() {
  py.test -n 3 --duration 20 pycket
}

do_coverage() {
  set +e
  # PyPy's pytest modifications clash with recent pytest-cov/coverage releases
  # So remove them on the CI.
  rm -rf ../pypy/*pytest*
  py.test --assert=plain -n 3 -k "$COVERAGE_TESTSUITE" --cov . --cov-report=term pycket
  codecov --no-fail -X gcov
  set -e
  echo '>> Testing whether coverage is over 80%'
  coverage report -i --fail-under=80 --omit='pycket/test/*','*__init__*'
}


do_translate() {
  ../pypy/rpython/bin/rpython -Ojit --batch targetpycket.py
  do_performance_smoke
}



do_performance_smoke() {
  _smoke() {
    RATIO=$1
    shift
    echo "> $1"
    raco make $1 >/dev/null
    RACKET_TIME=$($TIME_IT racket "$@")
    echo "    Racket took $RACKET_TIME"
    TARGET_TIME=$(awk "BEGIN { print ${RATIO} * ${RACKET_TIME}; }" )
    KILLME_TIME=$(awk "BEGIN { print ${TARGET_TIME} * 5; }")
    racket -l pycket/expand $1 2>/dev/null >/dev/null
    # $TIMEOUT -k$KILLME_TIME $KILLME_TIME ./pycket-c "$@"
    PYCKET_TIME=$($TIME_IT ./pycket-c "$@")
    echo "    Pycket took $PYCKET_TIME (should be < $TARGET_TIME)"
    [ "OK" = "$(awk "BEGIN { print ($PYCKET_TIME < $TARGET_TIME ? \"OK\" : \"NO\"); }")" ]
  }
  echo ; echo ">> Performance smoke test" ; echo
  echo ">>> Preparing racket files to not skew test"
  expand_rkt yes
  echo ">>> Smoke"
  _smoke 1.5 pycket/test/fannkuch-redux.rkt 10
  _smoke 0.7 pycket/test/triangle.rkt
  _smoke 1.8 pycket/test/earley.rkt
  _smoke 2.0 pycket/test/nucleic2.rkt
  _smoke 2.5 pycket/test/nqueens.rkt
  _smoke 2.5 pycket/test/treerec.rkt
  _smoke 0.5 pycket/test/hashtable-benchmark.rkt
  echo ; echo ">> Smoke cleared" ; echo
}

do_translate_nojit_and_racket_tests() {
  ../pypy/rpython/bin/rpython --batch targetpycket.py
  ../pypy/pytest.py pycket/test/racket-tests.py
}

############################################################

install_deps() {
  pip install pytest-xdist || pip install --user pytest-xdist
  if [ $TEST_TYPE = 'coverage' ]; then
    pip install codecov pytest-cov || pip install codecov pytest-cov
  fi
}

install_pypy() {
  # PYPY_PAK=pypy-c-jit-latest-linux64.tar.bz2
  # PYPY_URL=http://buildbot.pypy.org/nightly/release-4.0.x/pypy-c-jit-latest-linux64.tar.bz2
  PYPY_PAK=pypy-4.0.0-linux64.tar.bz2
  PYPY_URL=https://bitbucket.org/pypy/pypy/downloads/$PYPY_PAK

  wget $PYPY_URL
  tar xjf $PYPY_PAK
  # ln -s pypy-c-*-linux64 pypy-c
  ln -s pypy-4.0.0-linux64 pypy-c
  virtualenv -p pypy-c/bin/pypy .
}
install_racket() {
  ###
  #  Get and install Racket
  ###
  ## Debian
  # sudo add-apt-repository -y ppa:plt/racket
  # sudo apt-get update
  # sudo apt-get install -qq racket

  if [ "$(lsb_release -s -i)" = 'Debian' ]; then
    OS_PART=i386-linux-wheezy
  else
    OS_PART=x86_64-linux-precise
  fi

  case "$DLHOST" in
    utah)
      INSTALLER=racket-$RACKET_VERSION-$OS_PART.sh
      URL=http://www.cs.utah.edu/plt/snapshots/current/installers/$INSTALLER
      ;;
    northwestern)
      INSTALLER=racket-test-$RACKET_VERSION-$OS_PART.sh
      URL=http://plt.eecs.northwestern.edu/snapshots/current/installers/$INSTALLER
      ;;
    racket)
      INSTALLER=racket-$RACKET_VERSION-$OS_PART.sh
      URL=http://mirror.racket-lang.org/installers/$RACKET_VERSION/$INSTALLER
      ;;
    *) exit 1;;
  esac
  wget $URL
  sh $INSTALLER --in-place --dest racket
}

fetch_pypy() {
  ###
  #  Prepare pypy
  ###
  wget https://bitbucket.org/pypy/pypy/get/default.tar.bz2 -O `pwd`/../pypy.tar.bz2 || \
      wget https://bitbucket.org/pypy/pypy/get/default.tar.bz2 -O `pwd`/../pypy.tar.bz2
  tar -xf `pwd`/../pypy.tar.bz2 -C `pwd`/../
  mv ../pypy-pypy* ../pypy
}

prepare_racket() {
  raco pkg install -t dir pycket/pycket-lang/
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
    echo "Missing command"
    _help
    exit 1
fi

COMMAND="$1"
shift

case "$COMMAND" in
  prepare)
    echo "Preparing dependencies"
    install_pypy
    install_racket
    install_deps
    ;;
  install)
    echo "Preparing pypy and racket"
    fetch_pypy
    prepare_racket
    ;;
  test)
    export PYTHONPATH=$PYTHONPATH:../pypy:pycket
    if [ -z "$1" ]; then
        echo "Please tell what to test, see .travis.yml"
        _help
        exit 1
    else
      TEST_TYPE="$1"
      shift
    fi
    echo "Running $TEST_TYPE"
    do_$TEST_TYPE
    ;;
  *)
    _help
    ;;
esac
