#!/usr/bin/env python
import atexit
import codecs
import datetime
import email
import mailbox
import os
import pyinotify
import re
import subprocess
import time
import gi
gi.require_version('Notify', '0.7')
from gi.repository import Notify


#from gi._glib import GError

maildir = '/home/shaun/.mail/'
boxes = [
    ('UC_mail', 'INBOX/new'),
    ('gmail_mail', 'INBOX/new'),
    ('yahoo_mail', 'Inbox/new')]

Notify.init('Email notifier')

@atexit.register
def exiting():
    text = 'mail-notification exited'
    subprocess.call('notify-send -u critical "{}"'.format(text),
                    shell=True)

def new_mail(event):
    try:
        fd = codecs.open(event.pathname, 'r', 'utf-8')
        mail = mailbox.MaildirMessage(message=fd)
        fd.close()
    except FileNotFoundError:
        # Message has been moved by the spam filter
        #show('*** NO NOTIFICATION FOR SPAM MESSAGE ***', seconds=0)
        return
    try:
        From = header_decode(mail['From']).replace('"', '')
        To = header_decode(mail['To'])
        Subject = header_decode(mail['Subject'])
    except:
        # Keep running but show permanent notification
        text = 'mail-notification failed decoding header.'
        subprocess.call('notify-send -u critical "{}"'.format(text),
                        shell=True)
        return
    box = [box for box, folder in boxes if box in event.path][0]
    if box in To and box in From:
        text = ''.join(['Mail to self received.', '\n', box])
    else:
        text = ''.join([From, '\n', Subject, '\n', box])
    show(text)

def header_decode(text):
    if not text:
        return ''
    header = []
    for item in email.header.decode_header(text):
        try:
            text, encoding = item
        except TypeError:
            text, encoding = ('', None)
        if encoding:
            header.append(str(text.decode(encoding)))
        else:
            if isinstance(text, bytes):
                text = text.decode('utf-8')
            header.append(str(text))
    return ' '.join(header)

def show(text, seconds=10):
    #while 'locked' in subprocess.check_output('xscreensaver-command -time',
    #        shell=True).decode('utf-8'):
    #    time.sleep(5)
    #time.sleep(1)
    subprocess.call("notify-send -t {} \"{}\"".format(
        seconds * 1000, text), shell=True)
    #while True:
    #    try:
    #        text.show()
    #        print('{} Show notification'.format(datetime.datetime.now()))
    #        return
    #    except GError:
    #        print('{} Could not show notification, retrying soon'.format(
    #            datetime.datetime.now()))
    #        time.sleep(5)

def add_watch_helper(box, folder):
    wm.add_watch(os.path.join(os.path.expanduser(maildir), box, folder),
        pyinotify.IN_CREATE | pyinotify.IN_MOVED_TO)

wm = pyinotify.WatchManager()
notifier = pyinotify.Notifier(wm, new_mail)

[add_watch_helper(box, folder) for box, folder in boxes]

notifier.loop()
