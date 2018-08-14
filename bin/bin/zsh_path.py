#! /usr/bin/env python


from os import getcwd

import re

dirs = re.sub(r'/home/[a-zA-Z0-9]*/', '~/', getcwd()).split('/')

for i,d in enumerate(dirs):
    print(r"\%F{8}  » ")
    print('/%F\{#/{%d\}\}#\{%s\}',i,d)
