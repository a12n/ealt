#!/usr/bin/env python

from datetime import datetime
from ealt import Client

def on_commentary(timestamp, text):
    print datetime.fromtimestamp(timestamp).isoformat(), ':', text 

try:
    client = Client()
    client.connect(endpoint=("localhost", 8642))
    client.matcher.add_callback(tag="commentary", version=0, callback=on_commentary)
    client.run()
except KeyboardInterrupt:
    pass
    
