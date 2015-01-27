#/bin/sh

set -e

_help() {
  cat <<EOF
$0 <command>

command is one of

  prepare       Do anything necessary to allow installation
  install       Install direct prerequisites
  test <what>   Test (may include building, testing, coverage)
        tests         Run pytest tests
        coverage      Run pytest coverage report
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



COVERAGE_TESTSUITE='not test_larger and not (test_regexp and test_bug) and not test_or_parsing'
COVERAGE_HTML_DIR=pycket/test/coverage_report

############### test targets ################################
do_tests() {
  py.test -n 3 --duration 20 pycket
}


do_coverage() {
  py.test -n 3 -k "$COVERAGE_TESTSUITE" --cov . --cov-report=term pycket
  echo '>> Testing whether coverage is over 80%'
  coverage report -i --fail-under=80 --omit='pycket/test/*','*__init__*'
}

do_coverage_push() {
  # always succeed to allow coverage push on test failure
  py.test -n 3 -k "$COVERAGE_TESTSUITE" \
          --cov . --cov-report=html pycket || true
  # but fail if the report is not there
  [ -f .coverage -a \
       -d "$COVERAGE_HTML_DIR" -a \
       -f "$COVERAGE_HTML_DIR/index.html" ]
}


do_prepare_coverage_deployment() {
  [ -f .coverage ] || exit 1
  [ -d $COVERAGE_HTML_DIR ] || exit 1
  mv $COVERAGE_HTML_DIR /tmp
  rm -rf ./*
  cp -a "/tmp/$(basename "$COVERAGE_HTML_DIR")/"* .
  echo "web: vendor/bin/heroku-php-nginx" > Procfile
  echo '{}' > composer.json
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
    set -x
    raco make $1 >/dev/null
    RACKET_TIME=$($TIME_IT racket "$@")
    TARGET_TIME=$(awk "BEGIN { print ${RATIO} * ${RACKET_TIME}; }" )
    KILLME_TIME=$(awk "BEGIN { print ${TARGET_TIME} * 5; }")
    racket -l pycket/expand $1 2>/dev/null >/dev/null
    $TIMEOUT -k$KILLME_TIME $TARGET_TIME ./pycket-c "$@"
    set +x
  }
  echo ; echo ">> Performance smoke test" ; echo
  echo ">>> Preparing racket files to not skew test"
  expand_rkt yes
  echo ">>> Smoke"
  _smoke 1.2 pycket/test/fannkuch-redux.rkt 10
  _smoke 0.7 pycket/test/triangle.rkt
  _smoke 1.5 pycket/test/earley.rkt
  _smoke 0.5 pycket/test/nucleic2.rkt
  _smoke 2.5 pycket/test/nqueens.rkt
  _smoke 2.5 pycket/test/treerec.rkt
  echo ; echo ">> Smoke cleared" ; echo
}

do_translate_nojit_and_racket_tests() {
  ../pypy/rpython/bin/rpython --batch targetpycket.py
  ../pypy/pytest.py pycket/test/racket-tests.py
}

############################################################

install_deps() {
  pip install pytest-xdist pytest-cov cov-core coverage || \
      pip install --user pytest-xdist pytest-cov cov-core coverage
}

install_racket() {
  ###
  #  Get and install Racket
  ###
  ## Debian
  # sudo add-apt-repository -y ppa:plt/racket
  # sudo apt-get update
  # sudo apt-get install -qq racket
  ### Nightly from northwestern or utha
  VERSION=6.1.1.8
  if [ "$(lsb_release -s -i)" = 'Debian' ]; then
    INSTALLER=racket-test-$VERSION-i386-linux-wheezy.sh
  else
    INSTALLER=racket-test-$VERSION-x86_64-linux-precise.sh
  fi
  wget http://plt.eecs.northwestern.edu/snapshots/current/installers/$INSTALLER
  sh $INSTALLER --in-place --dest racket
  # wget http://www.cs.utah.edu/plt/snapshots/current/installers/racket-current-x86_64-linux-precise.sh
  # sh racket-current-x86_64-linux-precise.sh --in-place --dest racket
  ### Specific stable version from racket-lang
  # wget http://mirror.racket-lang.org/installers/6.1.1/racket-6.1.1-x86_64-linux-ubuntu-precise.sh
  # sh racket-6.1.1-x86_64-linux-ubuntu-precise.sh  --in-place --dest racket
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
        echo -n .
        set +e
        racket -l pycket/expand $F 2>/dev/null >/dev/null
        set -e
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
  prepare_coverage_deployment)
    do_prepare_coverage_deployment
    ;;
  *)
    _help
    ;;
esac
