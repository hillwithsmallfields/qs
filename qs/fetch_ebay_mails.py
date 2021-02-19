#!/usr/bin/python3

# following http://www.voidynullness.net/blog/2013/07/25/gmail-email-with-python-via-imap/

import datetime
import decouple
import email
import email.parser
import email.policy
import getpass
import imaplib
import os
import traceback
import re

import named_column_sheet

def start_of(s):
    return "|".join(str(s).strip().split('\r\n')).replace('-', '')[:244]
    # return str(s)
    # return "|".join(str(s).split())

def process_we_got_your_order(payload, result, message_date, number):
    print("     WGYO", start_of(payload))
    with open("/tmp/ebay/msg-wgyo-%s.html" % number, 'w') as outstream:
        outstream.write(payload)

    # look for <a href="https://rover.ebay.com/rover/...> for the item names
        
    count = 0
    seller = None
    item_name = None
    price = None
    p_and_p_price = None
    quantity = None
    item_total = None
    return count

def process_order_confirmed(payload, result, message_date, number):
    print("     OC", start_of(payload))
    with open("/tmp/ebay/msg-oc-%s.html" % number, 'w') as outstream:
        outstream.write(payload)
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
    with open("/tmp/ebay/msg-coyo-%s.txt" % number, 'w') as outstream:
        outstream.write(payload)
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

def process_order_confirmations(M):
    rv, data = M.search(None, "ALL")
    if rv != 'OK':
        print("No messages in here")
        return
    # print("mailbox data is", data)
    result = named_column_sheet.named_column_sheet(None, [
        'date',
        'seller',
        'item_name',
        'price',
        'p_and_p_price',
        'quantity',
        'item_total',
        'message_number'])
    message_list = data[0].decode('utf-8').split(' ')
    print("fetching", len(message_list), "messages")
    for message_number in message_list:
        # print("Getting message", message_number)
        try:
            rv, message_data = M.fetch(message_number, '(RFC822)')
            if rv != 'OK':
                print("Could not fetch message", message_number)
                continue
            acted = '    '
            message_contents = email.message_from_bytes(message_data[0][1], policy=email.policy.default)
            message_date = datetime.datetime.strptime(message_contents['Date'], "%a, %d %b %Y %H:%M:%S %z")
            message_subject = message_contents['Subject']
            message_from = message_contents['From']
            if message_from == "eBay <ebay@ebay.co.uk>" or message_from == "eBay <ebay@ebay.com>":
                counted = 0
                for s in handlers.keys():
                    if s in message_subject:
                        handler = handlers[s]
                        acted = 'm   '
                        for part in message_contents.walk():
                            if part.get_content_type() == handler[0]:
                                counted = handler[1](part.get_content(), result, message_date, message_number)
                                acted = 'p %02d' % counted 
                                break
                        if counted != 0:
                            break
            print(acted, "message %s: from %s on %s: %s" % (message_number, message_from, message_date, message_subject))
        except Exception as ex:
            print("error", ex)
            traceback.print_exc()
    result.write_csv("/tmp/orders.csv", suppress_timestamp=True)

def main():
    os.makedirs("/tmp/ebay", exist_ok=True)
    M = imaplib.IMAP4_SSL('imap.gmail.com')
    try:
        M.login(decouple.config('EBAY_EMAIL_ADDRESS'), getpass.getpass())
    except imaplib.IMAP4.error as login_error:
        print("failed to log in to email", login_error)
        return

    # rv, mailboxes = M.list()
    # if rv == 'OK':
    #     print("Mailboxes", ", ".join([str(mb) for mb in mailboxes]))

    rv, data = M.select("Shopping/Ordered")
    if rv == 'OK':
        process_order_confirmations(M)
    else:
        print("Could not access mailbox")
    M.close()
        
if __name__ == '__main__':
    main()
