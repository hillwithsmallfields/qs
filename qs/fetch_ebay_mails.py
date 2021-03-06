#!/usr/bin/python3

# following http://www.voidynullness.net/blog/2013/07/25/gmail-email-with-python-via-imap/

import argparse
import datetime
import decouple
import email
import email.parser
import email.policy
import getpass
import imaplib
from lxml import etree
import os
import os.path
import traceback
import re

import add_purchase_details
import named_column_sheet

def start_of(s):
    return "|".join(str(s).strip().split('\r\n')).replace('-', '')[:244]

def process_we_got_your_order(payload, result, message_date, number):

    tree = etree.HTML(payload)

    # with open("/tmp/parsed-%s.html" % number, 'wb') as dumpstream:
    #     dumpstream.write(etree.tostring(tree, pretty_print=True, method='html'))

    h1_a_texts = tree.xpath("//h1/a/text()")

    item_name = "............."
    seller = ""
    next_is_seller = False
    
    for text0 in h1_a_texts:
        text = text0.strip('. ')

        if next_is_seller:
            seller = text
            print("  seller", seller, "sold", item_name, "at price", item_price, "item number", item_number, "order number", order_number)
            result.add_row({
                'timestamp': message_date,
                'date': message_date.date(),
                'seller': seller,
                'item_name': item_name,
                # 'price': item_price,
                # 'p_and_p_price': p_and_p_price,
                # 'quantity': quantity,
                'item_total': item_price,
                'message_number': number})
            next_is_seller = False
        else:
            if item_name in text:
                next_is_seller = True
            item_name = text

            if next_is_seller:
                ancestor = text0
                for i in range(5):
                    ancestor = ancestor.getparent()
                bolds = ancestor.xpath("tr/td/p/b")
                fields = {}
                for bold in bolds:
                    fields[bold.xpath("text()")[0]] = bold.getparent().xpath("text()")[1].strip(': £')
                item_price = fields['Item price'].split()[0] if 'Item price' in fields else None
                item_number = fields['Item number'] if 'Item number' in fields else None
                order_number = fields['Order number'] if 'Order number' in fields else None

            
    count = 0
    # seller = None
    # item_name = None
    price = None
    p_and_p_price = None
    quantity = None
    item_total = None
    return count

def process_order_confirmed(payload, result, message_date, number):
    print("     OC", start_of(payload))

    tree = etree.HTML(payload)

    with open("/tmp/parsed-%s.html" % number, 'wb') as dumpstream:
        dumpstream.write(etree.tostring(tree, pretty_print=True, method='html'))

    h1_a_texts = tree.xpath("//h1/a/text()")

    item_name = None
    seller = ""
    next_is_seller = False
    
    for text0 in h1_a_texts:
        text = text0.strip('. ')

        print("got h1_a text", text)

        if item_name and item_name in text:
            item_name = text
            print("got longer repeat:", text)

            ancestor = text0
            for i in range(5):
                ancestor = ancestor.getparent()
            bolds = ancestor.xpath("tr/td/p/b")
            fields = {}
            for bold in bolds:
                fields[bold.xpath("text()")[0]] = bold.getparent().xpath("text()")[1].strip(': £')
            print("fields are", fields)
            item_price = fields['Total'].split()[0] if 'Total' in fields else None
            item_number = fields['Item number'] if 'Item number' in fields else None
            result.add_row({
                'timestamp': message_date,
                'date': message_date.date(),
                # 'seller': seller,
                'item_name': item_name,
                # 'price': item_price,
                # 'p_and_p_price': p_and_p_price,
                # 'quantity': quantity,
                'item_total': item_price,
                'message_number': number})
        else:
            item_name = text
            
    count = 0
    seller = None
    item_name = None
    price = None
    p_and_p_price = None
    quantity = None
    item_total = None
    return count

def process_confirmation_of_your_order(payload, result, message_date, number):
    print("     COYO", start_of(payload))
    count = 0
    seller = None
    item_name = None
    price = None
    p_and_p_price = None
    quantity = None
    item_total = None
    for line in payload.split('\n'):
        line = line.strip()
        # print("line is", line, )
        matched = re.match(" *Seller: +([a-z0-9_]+)", line)
        if matched:
            seller = matched.group(1)
            item_name = None
            price = None
            p_and_p_price = None
            quantity = None
            item_total = None
            continue
        matched = re.match("Item name +(.+)", line)
        if matched:
            item_name = matched.group(1)
            price = None
            p_and_p_price = None
            quantity = None
            item_total = None
            continue
        matched = re.match("Price: +£([0-9.]+)", line)
        if matched:
            price = matched.group(1)
            continue
        matched = re.match("P&amp;P price: +£?(.+)", line)
        if matched:
            p_and_p_price = matched.group(1)
            continue
        matched = re.match("Quantity: +([0-9]+)", line)
        if matched:
            quantity = matched.group(1)
            continue
        matched = re.match("Item total: +£([0-9.]+)", line)
        if matched:
            item_total = matched.group(1)
            continue
        if line.startswith("-------------"):
            if seller and item_name and price and p_and_p_price and quantity and item_total:
                result.add_row({
                    'timestamp': message_date,
                    'date': message_date.date(),
                    'seller': seller,
                    'item_name': item_name,
                    'price': price,
                    'p_and_p_price': p_and_p_price,
                    'quantity': quantity,
                    'item_total': item_total,
                    'message_number': number})
                item_name = None
                count += 1
    return count

handlers = {
    "Confirmation of your order": ('text/plain', process_confirmation_of_your_order),
    "Order confirmed": ('text/html', process_order_confirmed),
    "Order Confirmed": ('text/html', process_order_confirmed),
    "We got your order": ('text/html', process_we_got_your_order)}

def parse_message(message_text, message_number, result, dump=None):
    message_contents = email.message_from_bytes(message_text,
                                                policy=email.policy.default)
    message_date = datetime.datetime.strptime(message_contents['Date'], "%a, %d %b %Y %H:%M:%S %z")
    message_subject = message_contents['Subject']
    message_from = message_contents['From']
    acted = '    '
    if message_from == "eBay <ebay@ebay.co.uk>" or message_from == "eBay <ebay@ebay.com>":
        counted = 0
        for s in handlers.keys():
            if s in message_subject:
                handler = handlers[s]
                acted = 'm   '
                for part in message_contents.walk():
                    if part.get_content_type() == handler[0]:
                        if dump:
                            with open(os.path.join(dump, "msg-%s.msg" % message_number), 'wb') as outstream:
                                outstream.write(message_text)
                        counted = handler[1](part.get_content(),
                                             result,
                                             message_date,
                                             message_number)
                        acted = 'p %02d' % counted
                        break
                if counted != 0:
                    break
    print(acted, "message %s: from %s on %s: %s" % (message_number, message_from, message_date, message_subject))

def process_order_confirmations(M, result, dump=None):
    rv, data = M.search(None, "ALL")
    if rv != 'OK':
        print("No messages in here")
        return
    # print("mailbox data is", data)

    message_list = data[0].decode('utf-8').split(' ')
    print("fetching", len(message_list), "messages")
    for message_number in message_list:
        try:
            rv, message_data = M.fetch(message_number, '(RFC822)')
            if rv != 'OK':
                print("Could not fetch message", message_number)
                continue
            parse_message(message_data[0][1],
                          message_number,
                          result,
                          dump)
        except Exception as ex:
            print("error", ex)
            traceback.print_exc()

def parse_messages_from_email(server, email_address, password, result, dump=None):
    M = imaplib.IMAP4_SSL(server)
    try:
        M.login(email_address, password)
    except imaplib.IMAP4.error as login_error:
        print("failed to log in to email", login_error)
        return

    rv, data = M.select("Shopping/Ordered")
    if rv == 'OK':
        process_order_confirmations(M, result, dump=dump)
    else:
        print("Could not access mailbox")
    M.close()

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--dump",
                        help="""Directory to dump messages to.""")
    parser.add_argument("--list-mailboxes", action='store_true')
    parser.add_argument("--files", nargs='+')
    parser.add_argument("--server", default='imap.gmail.com')
    parser.add_argument("--email-address")
    parser.add_argument("--output", default="ebay-ordered-items.csv")
    args = parser.parse_args()

    if args.list_mailboxes:
        rv, mailboxes = M.list()
        if rv == 'OK':
            print("Mailboxes", ", ".join([str(mb) for mb in mailboxes]))
        return

    if args.dump:
        os.makedirs(args.dump, exist_ok=True)

    result = named_column_sheet.named_column_sheet(None,
                                                   add_purchase_details.item_detail_columns)

    if args.files:
        i = 0
        for filename in args.files:
            with open(filename, 'rb') as instream:
                parse_message(instream.read(),
                              str(i),
                              result,
                              args.dump)
                i += 1

    else:
        parse_messages_from_email(args.server,
                                  (args.email_address
                                   or decouple.config('EBAY_EMAIL_ADDRESS')),
                                  (decouple.config('EBAY_EMAIL_PASSWORD', None)
                                   or getpass.getpass()),
                                  result, args.dump)

    result.write_csv(args.output, suppress_timestamp=True)

if __name__ == '__main__':
    main()
