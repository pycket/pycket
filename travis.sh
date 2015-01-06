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
  ../pypy/pytest.py --duration 20
  echo '>> Testing whether coverage is over 80%'
  coverage report -i --fail-under=80 --omit='pycket/test/*','*__init__*'
}

do_coverage() {
  ../pypy/pytest.py -v -k 'not test_larger' --cov . --cov-report=term
  # todo: generate html and store somewhere
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
  pip install pytest-cov cov-core coverage
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
  wget http://plt.eecs.northwestern.edu/snapshots/current/installers/racket-test-6.1.1.6-x86_64-linux-precise.sh
  sh racket-test-6.1.1.6-x86_64-linux-precise.sh --in-place --dest racket
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
    echo "Preparing pypy and pycket-lang"
    fetch_pypy
    raco pkg install -t dir pycket/pycket-lang/
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
