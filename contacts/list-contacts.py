#!/usr/bin/env python

import argparse
import re
import contacts_data

def combine(contacts):
    """Combine all contacts with the same address."""
    return contacts

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--flag", action='append')
    parser.add_argument("-g", "--group", action='append')
    parser.add_argument("-n", "--no-combine")
    parser.add_argument("-N", "--no-add-partners")
    parser.add_argument("-p", "--postal-addresses", action='store_true')
    parser.add_argument("-e", "--email-addresses", action='store_true')
    parser.add_argument("input")
    args = parser.parse_args()
    by_id, by_name = contacts_data.read_contacts(args.input)
    selected = []
    if args.flag:
        flags = re.compile("[" + "".join(args.flag) + "]")
        selected += [someone for someone in by_id.values()
                     if flags.search(someone['Flags'])]
    if args.group:
        groups = set(args.group)
        selected += [someone for someone in by_id.values()
                     if len((groups.intersection(someone['_groups_']))) > 0]
    if not args.no_add_partners:
        invited_ids = [whoever['ID'] for whoever in selected]
        for whoever in selected:
            for partner in whoever['Partners']:
                if partner not in invited_ids:
                    selected.append(by_id[partner])
    if not args.no_combine:
        selected = combine(selected)
    postal_addresses = args.postal_addresses
    email_addresses = args.email_addresses
    for contact in selected:
        if email_addresses:
            email = contact['Primary email']
            if email != "":
                print email + " <" + contact['_name_'] + ">"
        else:
            print contact['_name_']

if __name__ == "__main__":
    main()
