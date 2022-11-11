#!/usr/bin/env python3

# this script was obtained from https://github.com/ysangkok/w3c-validator-runner

# Copyright (c) 2020 Janus Troelsen

# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted.

# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
# REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
# INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
# LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
# OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.

import random
import io
import sys
import itertools
import subprocess
import argparse
import string
import json

VALIDATOR_PATH = "/usr/lib/cgi-bin/w3c-markup-validator/check"
URL_PREFIX = "http://tyskland.goxadidi.dk:8800/"

def id(N):
    return ''.join(random.choice(string.ascii_uppercase + string.digits) for x in range(N)).encode("us-ascii")

def callvalidator(uri, mode):
    if "get" in mode:
        env = {"QUERY_STRING": "uri=" + URL_PREFIX + uri, "REQUEST_METHOD": "GET"}
        s = subprocess.check_output(VALIDATOR_PATH, env=env)
    else:
        with open(uri,"rb") as f:
            inputdata = f.read()
        i = 1
        boundary = id(i) 
        while boundary in inputdata:
            i += 1
            boundary = id(i)
        outputformat = b'json' if "json" in mode else b'html'
        data = b"".join([b'------' + boundary + b'\r\n' + x for x in [
            b'Content-Disposition: form-data; name="fragment"\r\n\r\n' + inputdata + b'\r\n', 
            b'Content-Disposition: form-data; name="prefill"\r\n\r\n0\r\n', 
            b'Content-Disposition: form-data; name="output"\r\n\r\n' + outputformat + b'\r\n', 
            b'Content-Disposition: form-data; name="doctype"\r\n\r\nInline\r\n',
            b'Content-Disposition: form-data; name="prefill_doctype"\r\n\r\nhtml401\r\n',
            b'Content-Disposition: form-data; name="group"\r\n\r\n0\r\n']]) + b'------' + boundary + b'--\r\n'
        env = {"CONTENT_LENGTH": str(len(data)), "REQUEST_METHOD": "POST", "CONTENT_TYPE": "multipart/form-data; boundary=----" + boundary.decode("us-ascii")}
        s = val(data, env)
        
    return s

def val(postdata, newenv):
    p = subprocess.Popen([VALIDATOR_PATH], env=newenv, stderr=subprocess.PIPE, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    return p.communicate(postdata)[0]

def filterhtml(txt):
    import lxml.etree
    import lxml.html

    ET = lxml.html.document_fromstring(txt)
    
    for i in itertools.chain(ET.xpath(".//*[@id='results']"), ET.xpath(".//*[@id='result']"), ET.xpath(".//*[@id='fatal-errors']")):
        for j in i:
            c = io.BytesIO()
            lxml.etree.ElementTree(j).write(c)
            yield c.getvalue()
            if b"The document located at" in c.getvalue(): return
            if b"The uploaded document" in c.getvalue(): return
            
def w3mrender(htmltext):
    stdout, stderr = subprocess.Popen(["w3m", "-dump", "-T", "text/html"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate(htmltext)
    return stdout

def checkheaderforerror(data):
    data = data.partition(b"\n\n")[0]
    data = data.split(b"\n")
    headers = dict([tuple([x.strip() for x in i.partition(b":")[0::2]]) for i in data])
    #print(headers)
    return headers[b"X-W3C-Validator-Status"] == b"Invalid"
    
def fin(rawresult):          
    print(rawresult.decode("utf-8"))
    sys.exit(haserror)

def renderjson(rawresult):
    try:
      result = json.loads(rawresult.decode("utf-8"))
    except ValueError:
      return False
    errors = 0
    warnings = 0
    for msg in result['messages']:
        if 'lastLine' in msg:
            print('%(type)s: line %(lastLine)d: %(message)s' % msg)
        else:
            print('%(type)s: %(message)s' % msg)
        if msg['type'] == 'error':
            errors += 1
        else:
            warnings += 1
    return True

def renderhtml(rawresult):
          rawresult = list(filterhtml(rawresult))
          rawresult = b"\n".join(rawresult)
          rawresult = w3mrender(rawresult)
          return rawresult
if __name__ == "__main__":
    par = argparse.ArgumentParser(description="Default: --renderjson")
    par.add_argument("--renderhtml",help="Upload to validator. Parse validator HTML.", const="renderhtml", action="store_const", dest="mode")
    par.add_argument("--rawhtml",   help="Upload to validator. Do not parse returned HTML.", const="rawhtml", action="store_const", dest="mode")
    par.add_argument("--gethtml",   help="Make validator download.        Parse validator HTML.", const="gethtml", action="store_const", dest="mode")
    par.add_argument("--rawget",    help="Make validator download. Do not parse validator HTML.", const="rawget", action="store_const", dest="mode")
    par.add_argument("--renderjson",help="Upload to validator.        Parse returned JSON.", const="renderjson", action="store_const", dest="mode")
    par.add_argument("--rawjson",   help="Upload to validator. Do not parse returned JSON.", const="rawjson", action="store_const", dest="mode")
    par.add_argument("uri",         help="File or URI to validate. URI if the validator is downloading, file when it's not.")
    parsed = par.parse_args()
    if not parsed.mode: parsed.mode = "renderjson"

    rawresult = callvalidator(parsed.uri, parsed.mode)
    haserror = checkheaderforerror(rawresult)
    if "raw" not in parsed.mode:
        rawresult = rawresult.partition(b"\n\n")[2]
        if "json" not in parsed.mode:
          fin(renderhtml(rawresult))
        elif "renderjson" == parsed.mode:
          res = renderjson(rawresult)
          if not res:
            fin(renderhtml(rawresult))
        else:
          fin(rawresult)
    else:
        fin(rawresult)
