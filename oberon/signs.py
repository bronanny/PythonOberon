

def py2signed(i, width=32):
  assert width > 0
  width -= 1
  b = 2**width
  if not (-b <= i < b):
    raise ValueError
  if i >= 0:
    return i
  return b + i + (1 << width)


def signed2py(i, width=32):
  assert width > 0
  b = 2**width
  if not (0 <= i < b):
    raise ValueError
  if i < b / 2:
    return i
  return i - b
  

class binary_addressing_mixin(object):

  def __getitem__(self, n):
    if isinstance(n, tuple):
      if len(n) != 2:
        raise IndexError('Must pass only two indicies.')
      start, stop = n
      return self._mask(stop, start - stop)

    if isinstance(n, slice):
      if n.step:
        raise TypeError('Slice with step not supported.')
      stop = n.stop or 0 # It might be None.
      return self._mask(stop, n.start - stop)

    return bool(self >> n & 1)

  def _mask(self, stop, n):
    if n < 0:
      raise IndexError('Indexes should be left-to-right.')
    if not n:
      raise IndexError('Zero bits.')
    return type(self)(self >> stop & (2**n - 1))


class bint(binary_addressing_mixin, int): pass
class blong(binary_addressing_mixin, long): pass


if __name__ == '__main__':
  W = 4
  for i in range(-10, 10):
    print '%2i' % i,
    try: s = py2signed(i, W)
    except ValueError: print
    else: print '  ->  %04s aka %i  ->  %2i' % (
      bin(s)[2:], s, signed2py(s, W)
      )
