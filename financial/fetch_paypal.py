#!/usr/bin/python3

import argparse
import decouple
import requests

# see https://developer.paypal.com/docs/api/overview/

def main():
    parser = argparse.ArgumentParser()
    args = parser.parse_args()
    client_id = decouple.config('PAYPAL_CLIENT_ID')
    secret = decouple.config('PAYPAL_SECRET')
    print("client_id", client_id, "secret", secret)
    response = requests.get("https://api-m.sandbox.paypal.com/v1/oauth2/token",
                            auth=(client_id, secret),
                            headers={'Accept': 'application/json',
                                     'Accept-Language': 'en_US'},
                            data="grant_type=client_credentials")
    print("response", response)
    as_json = response.json()
    print("result as json", as_json)

if __name__ == '__main__':
    main()
