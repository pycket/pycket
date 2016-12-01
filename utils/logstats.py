
import re
import sys

from collections import Counter

def oneof(strs):
    return "(?:%s)" % "|".join(strs)

BRIDGE_REGEX = r"# bridge out of Guard (0x[0-9a-fA-f]*) with \d* ops"
bridge_regex = re.compile(BRIDGE_REGEX)

def main(args):
    for fname in sorted(args):

        with open(fname, 'r') as infile:
            data = infile.read()

        bridge_ids = bridge_regex.findall(data)

        all_ids = oneof(bridge_ids)

        guard_regex = r"\+\d*: (guard_[^(]*)\(.*descr=<Guard%s>\).*" % all_ids
        guard_regex = re.compile(guard_regex)
        guards = guard_regex.findall(data)

        counter = Counter(guards)
        print(fname)
        for k, v in sorted(counter.items(), key=lambda x: x[0]):
            print("{:<20} {}".format(k + ":", v))
        print("")

if __name__ == '__main__':
    main(sys.argv[1:])

