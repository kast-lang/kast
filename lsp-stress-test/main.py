import os
os.chdir(os.path.dirname(__file__))

init = open('init.json').read()
change = open('change.json').read()


def print_req(req):
    print(f"Content-Length: {len(req)}\n\n{req}", end='')


print_req(init)
while True:
    print_req(change)
