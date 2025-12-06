import os
import time
import sys
os.chdir(os.path.dirname(__file__))

filename = 'src/util/kast_util.ml'

initialize = open('init.json').read()
didOpen = open('open.json').read().replace('examples/hello.ks', filename)
didChange = open('change.json').read().replace('examples/hello.ks', filename)


def print_req(req):
    print(f"Content-Length: {len(req)}\n\n{req}", end='', flush=True)


print_req(initialize)
print_req(didOpen)
idx = 0
while True:
    print_req(didChange)
    time.sleep(0.05)
    print(f'Sent change req #{idx}', file=sys.stderr)
    idx += 1
