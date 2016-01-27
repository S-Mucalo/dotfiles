#! /usr/bin/env python

import notmuch
import logging
import time

logging.basicConfig(level=logging.DEBUG)

def tag_message(msg, *tags):
    msg.freeze()
    for tag in tags:
        if tag[0] == '-':
            msg.remove_tag(tag[1:])
        elif tag[0] == '+':
            msg.add_tag(tag[1:])
        else:
            msg.add_tag(tag)
    msg.thaw()
    
def tag_search(db, search, *tags):
    q = notmuch.Query(db, search)
    count = 0
    for msg in q.search_messages():
        count += 1
        tag_message(msg, *tags)

    if count > 0:
        logging.debug('Tagging %d messages with (%s)' % (count, ' '.join(tags)))

_tags = []
start_time = time.time()

def tag(query, *tags):
        _tags.append( (query, tags) )

tag('path:gmail_mail/**', 'gmail')
tag('path:"gmail_mail/[Gmail]Sent Mail/**"', 'gmail-sent')

tag('path:UC_mail/**' 'UC_mail')
tag('path:"UC_mail/Sent Items/**"', 'UC_mail-sent')
tag('path:"UC_mail/Deleted Items"', '+deleted')


tag('path:yahoo_mail/**', 'yahoo_mail')
tag('path:yahoo_mail/Sent/**', 'yahoo_mail-sent')
tag('path:yahoo_mail/Trash/**', '+deleted')


tag('to:petsc-users  at mcs.anl.gov', 'petsc-users', 'lists', '-inbox', '-new','-unread') # Works
tag('to:fipy at nist.gov', 'fipy', 'lists', '-inbox', '-new', '-unread') # Works

db = notmuch.Database(mode=notmuch.Database.MODE.READ_WRITE)
q_new = notmuch.Query(db, 'tag:new')

n_msgs = q_new.count_messages() # approximate

    
start_time = time.time()

for query, tags in _tags:
    tag_search(db, '%s' % query, *tags)

db.close()

end_time = time.time()
logging.info('Sorted %d messages in %1.2f seconds' % (n_msgs, end_time - start_time))
