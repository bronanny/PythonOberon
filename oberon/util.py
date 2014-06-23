from math import log, floor
from struct import pack, unpack
from signs import blong, bint, signed2py


word = blong


def _signed(n, bits=16):
  limit = 2**bits
  if -limit < n < limit:
    q = ((n < 0) << (bits - 1)) + abs(n)
    return blong(q)[bits:]
  raise ValueError


def bits2signed_int(i):
  if not isinstance(i, (bint, blong)):
    raise ValueError("Must be bit-addressable int.")
  n = len(i)
  if not n:
    raise ValueError("Must have non-zero length. %r" % (i,))
  n -= 1
  return (-1 if i[n] else 1) * i[n:]


def encode_float(f):
  return bits2signed_int(word(unpack('>I', pack('>f', f))[0]))

def decode_float(f):
  return unpack('>f', pack('>I', _signed(f, 32)))[0]

def encode_set(s, size=32):
  return sum(1 << n for n in range(size) if n in s)

def decode_set(i, size=32):
  w = blong(i)
  return {n for n in range(size) if w[n]}


##def log2(x):
##  y = 0
##  while x > 1:
##    x /= 2
##    y += 1
##  return y


def log2(x):
  return int(floor(log(x, 2)))


def word_print(it):
  b = bin(it)[2:]
  print (32 - len(b)) * '0' + b


if __name__ == '__main__':
  for n in xrange(2**32 / 2 - 1):
    print n, decode_set(n)
