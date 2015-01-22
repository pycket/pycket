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

############### test targets ################################
do_tests() {
  py.test -n 3 --duration 20 pycket
}

do_coverage() {
  py.test \
      -n 3 -k 'not test_larger and not test_bug and not test_or_parsing' \
      --cov . --cov-report=term --cov-report=html \
      pycket
  echo '>> Testing whether coverage is over 80%'
  coverage report -i --fail-under=80 --omit='pycket/test/*','*__init__*'
}

do_prepare_coverage_deployment() {
  COVERAGE_HTML_DIR=pycket/test/coverage_report
  [ -f .coverage ] || exit 1
  [ -d $COVERAGE_HTML_DIR ] || exit 1
  mv $COVERAGE_HTML_DIR /tmp
  rm -rf ./*
  cp -a "/tmp/$(basename "$COVERAGE_HTML_DIR")/"* .
}

do_translate() {
  ../pypy/rpython/bin/rpython -Ojit --batch targetpycket.py
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

  PRIVATE_MODULES=$(racket -e '(displayln (path->string (path-only (collection-file-path "stx.rkt" "racket/private"))))')
  find $PRIVATE_MODULES -type f -name \*.rkt | \
      while read F; do
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
