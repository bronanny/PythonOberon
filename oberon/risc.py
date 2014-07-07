'''
This is a simple eumulator for the RISC cpu developed by Prof. Wirth for
the Oberon OS (and compiler.)
'''
from assembler import dis


F = 2**32-1
IO_RANGE = 0xFFFFFFC0


def py2signed(i, width=32):
  '''
  Return a two's-complement version of a Python int of the given bit width.
  '''
  assert width > 0
  width -= 1
  b = 2**width
  if not (-b <= i < b):
    raise ValueError
  if i >= 0:
    return i
  return b + i + (1 << width)


def signed2py(i, width=32):
  '''
  Return a Python version of a two's-complement int of the given bit width.
  '''
  assert width > 0
  b = 2**width
  if not (0 <= i < b):
    raise ValueError
  if i < b / 2:
    return i
  return i - b
  

class binary_addressing_mixin(object):
  '''
  A mixin to allow an ``int`` or ``long`` to be addressed bit-wise.

  Single index gives a boolean, two gives a new int (n >= 0) of that width.

  You can use the following::

      n[i]    single index
      n[j:i]  slice
      n[j, i] two-tuple

  ``n[i]`` gives a ``bool`` while ``n[j:i]`` gives a new number created
  from the ``j - i`` bits beginning with the bit at ``i`` and including
  bits to the left (more significant.)

  For example (see ``blong`` below)::

      >>> b = blong(23)
      >>> bin(b)
      '0b10111'
      >>> b[3]
      False
      >>> b[0]
      True
      >>> b[4:2]
      1L

  '''

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


class blong(binary_addressing_mixin, long):
  '''
  A ``long`` that can be read bit-wise. See ``binary_addressing_mixin``.
  '''
  pass


class Trap(Exception):
  '''
  Generic trap mechanism.
  '''
  pass


class RISC(object):
  '''
  The RISC emulator.

  Written as a class rather than a single (large) function mostly so that
  the intermediate state and stages could be accessible to introspection.
  '''

  MT = 12 # Module Table register.

  def __init__(self, ram, PC=0):
    self.ram = ram
    self.PC = self.pcnext = PC
    self.R = [0] * 16
    self.H = 0
    self.N = self.Z = self.C = self.OV = 0
    self.io_ports = {}

  def cycle(self):
    '''
    Run a single instruction-processing cycle.

    This is the main interface to the emulator.  Call it repeatedly to
    run the computer.
    '''
    self.PC = self.pcnext
    instruction = self.ram[self.PC << 2]
    self.decode(instruction)
    self.what_are_we_up_to()
    self.control_unit()

  def decode(self, instruction):
    '''
    Decode the current instruction and set all the field attributes.

    ::

        31 28   24     20   16     12    8      4    0
        [0000 0000 | 0000 0000 | 0000 0000 | 0000 0000]
         pquv _ira   _irb [op]   |                _irc
               | |        |      [_________imm_______]
               _cc        [__________off_____________]

    '''
    self.IR = IR = blong(instruction)
    self.p = IR[31]
    self.q = IR[30]
    self.u = IR[29]
    self.v = IR[28]
    self.op = IR[20:16]
    self.ira = IR[28:24]
    self.irb = IR[24:20]
    self.irc = IR[4:0]
    self.cc = IR[27:24]
    self.imm = IR[16:0]
    self.off = signed2py(IR[20:0], 20)
    # I am guessing that this has to be (allowed to be) a negative value
    # otherwise there would be no way of jumping "backward" using a BR
    # instruction with a negative offset.

# Hmm, where did this come from?
#    self.jmp = IR[24:0]

    self.C0 = self.R[self.irc]

  def what_are_we_up_to(self):
    '''

    This is a long-winded way to figure out which operation an instruction
    specifies.  Rather than a long if-elif-else statement we set all flags
    every cycle.  The format of the code should make it easy to see that
    only one flag will be True for a given set of {(p, op), q, u}.

    '''
    self.MOV = (not self.p) and (self.op == 0)
    self.LSL = (not self.p) and (self.op == 1)
    self.ASR = (not self.p) and (self.op == 2)
    self.ROR = (not self.p) and (self.op == 3)
    self.AND = (not self.p) and (self.op == 4)
    self.ANN = (not self.p) and (self.op == 5)
    self.IOR = (not self.p) and (self.op == 6)
    self.XOR = (not self.p) and (self.op == 7)
    self.ADD = (not self.p) and (self.op == 8)
    self.SUB = (not self.p) and (self.op == 9)
    self.MUL = (not self.p) and (self.op == 10)
    self.DIV = (not self.p) and (self.op == 11)

    self.LDR = self.p and (not self.q) and (not self.u)
    self.STR = self.p and (not self.q) and self.u
    self.BR  = self.p and self.q

  def control_unit(self):
    '''
    The RISC unit has three very simple kinds of instructions, indicated
    by the two higest bits of an instruction.

    ::

        if p:
          if q:
            branch instruction...
          else:
            ram instruction...
        else:
          register instruction...

    '''
    if not self.p:
      self.register_instruction()
    elif self.q:
      self.branch_instruction()
      if self.pcnext == self.R[self.MT]:
        raise Trap(self.IR[8:4])
    else:
      self.ram_instruction()

  def Arithmetic_Logical_Unit(self):
    '''
    Perform arithmetic or logic.
    '''
    B = self.R[self.irb]

    # Here's how negative immediate values are stored in the instruction and
    # regenerated in the cpu. In the ORGX module the instruction is created
    # like so:
    #
    #  PROCEDURE Put1(op, a, b, im: LONGINT);
    #  BEGIN (*emit format-1 instruction,  -10000H <= im < 10000H*)
    #    IF im < 0 THEN INC(op, 1000H) END ;  (*set v-bit*)
    #    code[pc] := (((a+40H) * 10H + b) * 10H + op) * 10000H + (im MOD 10000H); INC(pc)
    #  END Put1;
    #
    # If the immediate value is negative the V bit in the instruction is set.
    # In any event the immediate value has it's high sixteen bits masked off
    # (modulus 0x10000 effects this.)  For example, -23 looks like this,
    # bit-wise:
    #
    #   11111111111111111111111111101001
    #
    # And after truncating the high bits:
    #
    #                   1111111111101001
    #
    # This is the bit pattern that gets stored in the instruction immediate
    # field.
    #
    # The statement immediately below reconstructs the negative 32-bit value
    # if the V bit is set in the instruction, otherwise it simply passes
    # through the given immediate value. (This happens only if the Q bit
    # is set, otherwise C1 is just set to C0.)

    C1 = self.C1 = (
      (0b11111111111111110000000000000000 | self.imm)
      if self.v else
      self.imm
      ) if self.q else self.C0

    if self.MOV:
      # (q ? (~u ? {{16{v}}, imm} : {imm, 16'b0}) :
      if self.q:
        res = C1 if not self.u else (self.imm << 16)
      else:
        # (~u ? C0 : ... )) :
        if not self.u:
          res = self.C0
        else:
          # ... (~v ? H : {N, Z, C, OV, 20'b0, 8'b11010000})
          if not self.v:
            res = self.H
          else:
            res = (
              self.N << 31 |
              self.Z << 30 |
              self.C << 29 |
              self.OV << 28 |
              0b11010000
              )

    # Bit-wise logical operations

    elif self.LSL:
      res = B << C1
    elif self.ASR or self.ROR:
      res = B >> C1 # FIXME not quite right is it?
    elif self.AND:
      res = B & C1
    elif self.ANN:
      res = B & (F ^ C1)
    elif self.IOR:
      res = B | C1
    elif self.XOR:
      res = B ^ C1

    # For the arithmetical operators we must convert to Python ints to
    # correctly handle negative numbers.
    B = signed2py(B)
    C1 = signed2py(C1)

    if self.ADD:
      res = B + C1 + (self.u and self.C)
      res = self._check_overflow(res)

    elif self.SUB:
      res = B - C1 - (self.u and self.C)
      res = self._check_overflow(res)

    elif self.MUL:
      product = B * C1
      self.product = blong(py2signed(product, 64))
      res = self.product[32:0]

    elif self.DIV:
      res, remainder = divmod(B, C1)
      res = py2signed(res)
      self.remainder = py2signed(remainder)

    return res if isinstance(res, blong) else blong(res)

  def _check_overflow(self, res, bits=33):
    self.OV = False
    try:
      return py2signed(res)
    except ValueError:
      self.OV = True
      return blong(py2signed(res, bits))[32:0]

  def register_instruction(self):
    '''
    Perform a register instruction and update status flags (uses the ALU.)
    '''
    self.pcnext = self.PC + 1
    regmux = self.Arithmetic_Logical_Unit()
    self.R[self.ira] = regmux[32:0]
    self.N = regmux[31]
    self.Z = regmux == 0
    if self.ADD | self.SUB:
      self.C = regmux[32]
    self.H = (self.product[64:32] if self.MUL
              else self.remainder if self.DIV
              else self.H)

  def branch_instruction(self):
    '''
    Perform a branch instruction by updating ``pcnext`` to take effect in
    the next cycle.
    '''
    S = self.N ^ self.OV
    if not (
      self.IR[27] ^
      ((self.cc == 0) & self.N |
       (self.cc == 1) & self.Z |
       (self.cc == 2) & self.C |
       (self.cc == 3) & self.OV |
       (self.cc == 4) & (self.C|self.Z) |
       (self.cc == 5) & S |
       (self.cc == 6) & (S|self.Z) |
       (self.cc == 7)
       )
      ):
      self.pcnext = self.PC + 1
      return

    if self.v: # Save link
      self.R[15] = self.PC + 1

    if self.u:
      self.pcnext = int(self.off + self.PC + 1)
# I don't remember where I got this from...
#    elif self.IR[5]:
#      self.pcnext = int(self.irc)
    else:
      self.pcnext = int(blong(self.C0)[20:2])

  def ram_instruction(self):
    '''
    Read or write RAM (or I/O ports.)
    '''
    self.addr = addr = int(self.R[self.irb] + self.off)
    if addr >= IO_RANGE:
      self.io(self, addr - IO_RANGE)
    elif self.LDR:
      self.R[self.ira] = (self.ram.get_byte(addr) if self.v
                          else self.ram[addr])
    elif self.v:
      self.ram.put_byte(addr, self.R[self.ira] & 255)
    else:
      self.ram[addr] = self.R[self.ira]
    self.pcnext = self.PC + 1

  def io(self, port):
    '''
    Read or write I/O ports.
    '''
    device = self.io_ports.get(port)
    if not device:
      raise Trap('no device at port 0x%x (aka %i)' % (port, port))
    if self.LDR:
      self.R[self.ira] = device.read()
    else:
      device.write(self.R[self.ira])

  def view(self):
    '''
    Helper method; prints out a simple display of CPU state.
    '''
    kw = self.__dict__.copy()
    kw['A'] = self.R[self.ira]
    print '- ' * 40
    print 'PC: [0x%(PC)04x] = 0x%(IR)08x =' % kw, dis(int(self.IR))
    print
    if self.STR:
      print 'Storing', '[0x%(addr)04x] <- R%(ira)i = 0x%(A)08x' % kw
      print
    elif self.LDR:
      print 'loading', 'R%(ira)i <- [0x%(addr)04x]' % kw
      print

    for i in range(0, 16, 2):
      reg0, reg1 = self.R[i], self.R[i + 1]
      print 'R%-2i = 0x%-8x' % (i + 1, reg1),
      print 'R%-2i = 0x%-8x' % (i, reg0)
    print


class ByteAddressed32BitRAM(object):
  '''
  Imitate a RAM that is addressed per-byte, reads and writes four-byte
  words by default, and can be accessed per-byte if desired.
  '''

  def __init__(self):
    self.store = {}

  def get(self, addr):
    '''
    Return the word at ``addr``, which must be a multiple of 4.
    '''
    word_addr, byte_offset = divmod(addr, 4)
    assert not byte_offset, repr(addr)
    return self.store[word_addr]

  __getitem__ = get

  def put(self, addr, word):
    '''
    Write the word to ``addr``, which must be a multiple of 4.
    '''
    assert 0 <= word <= F, repr(word)
    word_addr, byte_offset = divmod(addr, 4)
    assert not byte_offset, repr(addr)
    self.store[word_addr] = word

  __setitem__ = put

  def get_byte(self, addr):
    '''
    Return the byte at ``addr``.
    '''
    word_addr, byte_offset = divmod(addr, 4)
    word = self.store[word_addr]
    return (word >> (8 * byte_offset)) & 255

  def put_byte(self, addr, byte):
    '''
    Write the byte to ``addr``.
    '''
    if isinstance(byte, str):
      byte = ord(byte[:1])
    if not (0 <= byte < 256):
      raise ValueError("byte out of range: %i" % (byte,))

    word_addr, byte_offset = divmod(addr, 4)
    n = 8 * byte_offset # How many bits to shift.
    byte <<= n

    try: # Get the current memory contents, if any.
      word = self.store[word_addr]
    except KeyError: # nothing there yet so
      pass # just store shifted byte, or
    else: # merge word and shifted byte
      mask = F ^ (255 << n)
      # mask should now be one of:
      # 0b11111111111111111111111100000000
      # 0b11111111111111110000000011111111
      # 0b11111111000000001111111111111111
      # 0b00000000111111111111111111111111
      # Now AND the mask with the memory word to clear the bits for the
      # pre-shifted byte, and OR the result with same.
      byte |= word & mask
    self.store[word_addr] = byte


def _format_bin(n, width=32, literal=True):
  negative = n < 0
  n = abs(n)
  s = bin(n)[2:]
  d = width - len(s)
  s = [s]
  if d > 0: s.append('0' * d)
  if literal: s.append('0b')
  if negative: s.append('-')
  return ''.join(reversed(s))


if __name__ == '__main__':
  from assembler import Mov_imm, Add, Lsl_imm, T_link

  memory = ByteAddressed32BitRAM()
  for addr, instruction in enumerate((

    # A very simple program to compute ((1 + 1) << 2) - 2

    Mov_imm(8, 1), #    00: Mov R8 <- 1
    Mov_imm(1, 7), #    01: Mov R1 <- 7
    Add(1, 1, 8), #     02: Add R1 <- R1 + R8
    Mov_imm(2, -2), #   03: Mov R2 <- -2
    Add(1, 1, 2), #     04: Add R1 <- R1 + R2
    Lsl_imm(1, 1, 2), # 05: Lsl R1 <- R1 << 2

    T_link(1), #        06: BR to R1 (infinite loop)

    # At this point in execution R1 contains 24 (0x18) which is divided
    # by 4 (bytes per instruction) giving RAM address 6, so the RISC
    # emulator enters an infinite loop.

    )):
    memory.put(addr * 4, int(instruction))

  # Print out a view of the program in RAM.
  for address in sorted(memory.store):
    instruction = memory[address * 4]
    print (
      '%02i: '
      '%10i '
      '0x%08x '
      '%s'
      ) % (
        address,
        instruction,
        instruction,
        _format_bin(instruction),
        ), dis(instruction)

  risc_cpu = RISC(memory)
  for _ in range(10):
    risc_cpu.cycle()
    risc_cpu.view()
