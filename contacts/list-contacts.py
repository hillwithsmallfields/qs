import argparse
import re
import contacts_data

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--flags")
    parser.add_argument("-g", "--groups")
    parser.add_argument("-n", "--no-combine")
    parser.add_argument("input")
    args = parser.parse_args()
    by_id, _ = contacts_data.read_contacts(args.input)
    selected = set([])
    if args.flags:
        flags = re.compile("[" + args.flags + "]")
        selected += [someone for someone in by_id.values()
                     if flags.search(someone['Flags'])]
    if args.groups:
        flags = re.compile("(" + '|'.join(args.groups.split(',')) + ")")
        selected += [someone for someone in by_id.values()
                     if (flags.search(someone['Group Membership'])
                         or flags.search(someone['Organizations'])
                         or flags.search(someone['Other groups']))]
    if not args.no_combine:
        combine()

if __name__ == "__main__":
    main()
