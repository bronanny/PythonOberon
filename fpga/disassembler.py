from myhdl import intbv
from util import ops_rev, cmps


def dis(n):
  IR = intbv(n)[32:]
  p, q = IR[31], IR[30]
  if not p:
    if not q:
      return dis_F0(IR)
    return dis_F1(IR)
  if not q:
    return dis_F2(IR)
  if IR[29]:
    return dis_F3(IR)
  return dis_F3imm(IR)


def opof(op):
  return ops_rev[int(op)]


def dis_F0(IR):
  op, ira, irb, irc = IR[20:16], IR[28:24], IR[24:20], IR[4:0]
  u = IR[29]
  if not op: # Mov
    return '%s R%i <- R%i (u: %s)' % (
      opof(op), ira, irc, u,
      )
  return '%s R%i <- R%i R%i (u: %s)' % (
    opof(op),
    ira, irb, irc,
    u,
    )


def dis_F1(IR):
  op, ira, irb = IR[20:16], IR[28:24], IR[24:20]
  u = IR[29]
  v = IR[28]
  imm = IR[16:0]
  if not op: # Mov
    return '%s R%i <- %i (u: %s, v: %s)' % (
      opof(op), ira, imm, u, v)
  return '%s R%i <- R%i %i (u: %s, v: %s)' % (
    opof(op), ira, irb, imm, u, v)


def dis_F2(IR):
  op = 'Store' if IR[29] else 'Load'
  arrow = '->' if IR[29] else '<-'
  width = ' byte' if IR[28] else ''
  ira = IR[28:24]
  off = IR[20:0]
  return '%s R%i %s [0x%08x]%s' % (op, ira, arrow, off, width)


def dis_F3(IR):
  link = '_link' if IR[28] else ''
  invert = int(IR[27])
  cc = int(IR[27:24])
  op = cmps[cc, invert]
  irc = IR[4:0]
  return 'BR%s %s [R%i]' % (link, op, irc)


def dis_F3imm(IR):
  link = '_link' if IR[28] else ''
  invert = int(IR[27])
  cc = int(IR[27:24])
  op = cmps[cc, invert]
  off = IR[24:0]
  return 'BR%s %s [0x%08x]' % (link, op, off)


data = [int(n, 2) for n in '''\
01001110111010010000000000011000
10101111111000000000000000000000
10100000111000000000000000000100
10100001111000000000000000001000
10100010111000000000000000001100
10001101000000000000000000000101
10000000110100000000000000000000
10100000111000000000000000010100
10000000111000000000000000010100
11010001000001101011001001001100
10000000000000000000000000010000
01000000000010010000000000000000
11100001000000000000000000010100
10000000111000000000000000010100
11010001000001101100001101001100
10000000000000000000000000010000
11010001000001101100100001001100
01000000000010000000000000011100
10000001111000000000000000001000
10010010000000000000000000000000
01000000000010000000000000000001
10010011000100000000000000000000
01000001000110000000000000000001
00000100001010010000000000000011
11101001000000000000000000000010
01000100001010010000000000000000
11101001111111111111111111111000
11100001000000000000000000000101
10000000111000000000000000010100
11010001000001101101110101001100
10000000000000000000000000010000
10100000111000000000000000010100
11100111111111111111111111100111
10000000111000000000000000010100
11010001000001101111000101001100
10000000000000000000000000010000
01000000000010010000000000000000
11101001000000000000000000100111
01000000111010000000000000010000
10001101000000000000000000100010
01000001110110000000000000000000
11010111000000000000000000001100
10000000111000000000000000010000
11010001000001110001010101001100
01000000000010000000000000011100
10000001111000000000000000001000
01000010000000000000000000000110
10000011000100000000000000000000
01000001000110000000000000000100
10100011000000000000000000000000
01000000000010000000000000000100
01000010001010010000000000000001
11101001111111111111111111111010
10000000111000000000000000010000
11010001000001110010011001001100
10000001111000000000000000001100
10100001000000000000000000000000
10000000111000000000000000010000
11010001000001110011100101001100
01000001000000000000000000000000
10100001000000000000000000010000
10000000111000000000000000010000
11010001000001110100100101001100
01000001000000000000000000000000
10110001000000000000000000001101
10000000111000000000000000010000
11010001000001110101101101001100
01000001000000000000000000000000
10100001000000000000000000010100
10000000111000000000000000010100
11010001000001110111000001001100
10000001111000000000000000010000
10100001000000000000000000010000
10000000111000000000000000010000
10000001111000000000000000000100
10100000000100000000000000000000
11100111000000000000000000000101
10000000111000000000000000010100
11010001000001111001101001001100
10000000000000000000000000010000
10000001111000000000000000000100
10100000000100000000000000000000
10001111111000000000000000000000
01001110111010000000000000011000
11000111000000000000000000001111
01001110111010010000000000001100
10101111111000000000000000000000
10001101000000000000000000110000
10000000110100000000000000000000
10100000111000000000000000000100
10000000111000000000000000000100
11010001000010000001011101001100
10000000000000000000000000010000
10100000111000000000000000001000
10000000111000000000000000001000
01000000000010010000000000000000
11100001000000000000000000010100
10000000111000000000000000001000
11010001000010000011100001001100
10000001111000000000000000001000
11010001000010000100000101001100
01000000000010000000000000011100
01000001000110000000000000011100
10010010000000000000000000000000
01000000000010000000000000000001
10010011000100000000000000000000
01000001000110000000000000000001
00000100001010010000000000000011
11101001000000000000000000000010
01000100001010010000000000000000
11101001111111111111111111111000
11100001000000000000000000000101
10000000111000000000000000001000
11010001000010000101000101001100
10000000000000000000000000010000
10100000111000000000000000001000
11100111111111111111111111101001
10000000111000000000000000000100
11010001000010000110100001001100
10000000000000000000000000010100
10100000111000000000000000000100
10000000111000000000000000001000
01000000000010010000000000000000
11101001000000000000000000000011
10000000111000000000000000000100
01000000000010010000000000000000
11101001111111111111111111011011
10000000111000000000000000001000
10001111111000000000000000000000
01001110111010000000000000001100
11000111000000000000000000001111
01001110111010010000000000001100
10101111111000000000000000000000
10100000111000000000000000000100
10000000111000000000000000000100
11010001000010010000000101001100
10010000000000000000000000001101
01000000000010010000000000000000
11100001000000000000000000100100
10000000111000000000000000000100
11010001000010010001100001001100
10010000000000000000000000011100
01000000000010010000000000000000
11100001000000000000000000011100
10000000111000000000000000000100
11010001000010010011110001001100
10000000000000000000000000010100
10100000111000000000000000001000
10000000111000000000000000001000
01000000000010010000000000000000
11100001000000000000000000010100
10000000111000000000000000001000
11010001000010010110001101001100
10000001111000000000000000001000
11010001000010010110111001001100
01000000000010000000000000011100
01000001000110000000000000011100
10010010000000000000000000000000
01000000000010000000000000000001
10010011000100000000000000000000
01000001000110000000000000000001
00000100001010010000000000000011
11101001000000000000000000000010
01000100001010010000000000000000
11101001111111111111111111111000
11100001000000000000000000000101
10000000111000000000000000001000
11010001000010011000001001001100
10000000000000000000000000010000
10100000111000000000000000001000
11100111111111111111111111101001
11100111000000000000000000000010
01000000000000000000000000000000
10100000111000000000000000001000
11100111000000000000000000000010
01000000000000000000000000000000
10100000111000000000000000001000
10000000111000000000000000001000
10001111111000000000000000000000
01001110111010000000000000001100
11000111000000000000000000001111
01001110111010010000000000001100
10101111111000000000000000000000
10100000111000000000000000000100
10000000111000000000000000000100
11010001000010100011110101001100
10000000000000000000000000010100
10100000111000000000000000001000
10000000111000000000000000001000
01000000000010010000000000000000
11100001000000000000000000010100
10000000111000000000000000001000
11010001000010100110000001001100
10000001111000000000000000001000
11010001000010100110101101001100
01000000000010000000000000011100
01000001000110000000000000011100
10010010000000000000000000000000
01000000000010000000000000000001
10010011000100000000000000000000
01000001000110000000000000000001
00000100001010010000000000000011
11101001000000000000000000000010
01000100001010010000000000000000
11101001111111111111111111111000
11100001000000000000000000000101
10000000111000000000000000001000
11010001000010100111111101001100
10000000000000000000000000010000
10100000111000000000000000001000
11100111111111111111111111101001
10000000111000000000000000001000
10001111111000000000000000000000
01001110111010000000000000001100
11000111000000000000000000001111
01001110111010010000000000001000
10101111111000000000000000000000
01000000111010000000000000000100
10001101000000000000000010000011
01000001110110000000000000000000
11010111000000000000000000001100
10000000111000000000000000000100
11010001000010101110100101001100
01000001000000000000000000000000
10100001000000000000000000000000
10000000111000000000000000000100
11010001000010101111100001001100
10001101000000000000000000001001
10000001110100000000000000000000
10100001000000000000000000010100
10000000111000000000000000000100
11010001000010110000110001001100
01000001000000000000000000000000
10100001000000000000000000010000
10000000111000000000000000000100
10100000110100000000000000000000
10001111111000000000000000000000
01001110111010000000000000001000
11000111000000000000000000001111
01001110111010010000000000000100
10101111111000000000000000000000
10001101000000000000000000001110
10000000110100000000000000000000
11010001000010110110111001001100
10000000000000000000000000010100
10100000110100000000000000000000
10001111111000000000000000000000
01001110111010000000000000000100
11000111000000000000000000001111
01001110111010010000000000100000
10101111111000000000000000000000
10100000111000000000000000000100
10100001111000000000000000001000
10100010111000000000000000001100
10100011111000000000000000010000
10100100111000000000000000010100
01000000000000000000000000000000
10100000111000000000000000011000
01000000000000000000000000000000
10100000111000000000000000011100
10000000111000000000000000011000
01000000000010010000000000011011
11101101000000000000000000011010
10000000111000000000000000011000
10000001111000000000000000001100
00000001000010010000000000000001
11011010000011001001011000011100
10000001111000000000000000001000
00000000000110000000000000000000
10010000000000000000000000000000
01000000000010010000000000000000
11100110000000000000000000010001
10000000111000000000000000011000
01000001000010010000000000010111
11011010000011001010100000011100
10000001111000000000000000000100
00000000000110000000000000000000
10000001111000000000000000011000
10000010111000000000000000001100
00000010000110010000000000000010
11011010000011001011001100011100
10000010111000000000000000001000
00000001001010000000000000000001
10010001000100000000000000000000
10110001000000000000000000000000
10000000111000000000000000011000
01000000000010000000000000000001
10100000111000000000000000011000
11100111111111111111111111100011
10000000111000000000000000011000
01000001000010010000000000010111
11011010000011001101010100011100
10000001111000000000000000000100
00000000000110000000000000000000
10000001111000000000000000011100
10000010111000000000000000010100
00000010000110010000000000000010
11011010000011001101111000011100
10000010111000000000000000010000
00000001001010000000000000000001
10010001000100000000000000000000
10110001000000000000000000000000
10000000111000000000000000011000
01000000000010000000000000000001
10100000111000000000000000011000
10000000111000000000000000011100
01000000000010000000000000000001
10100000111000000000000000011100
10000000111000000000000000011100
10000001111000000000000000010100
00000001000010010000000000000001
11011010000011001111101100011100
10000001111000000000000000010000
00000000000110000000000000000000
10010000000000000000000000000000
01000000000010010000000000000000
11101001111111111111111111100100
10000000111000000000000000011000
01000001000010010000000000010111
11011010000011010000111000011100
10000001111000000000000000000100
00000000000110000000000000000000
01000001000000000000000000000000
10110001000000000000000000000000
10001111111000000000000000000000
01001110111010000000000000100000
11000111000000000000000000001111
01001110111010010000000000100000
10101111111000000000000000000000
10100000111000000000000000000100
10100001111000000000000000001000
10100010111000000000000000001100
10100011111000000000000000010000
10001101000000000000000001011100
10000000110100000000000000000000
10100000111000000000000000011100
10000000111000000000000000011100
11010001000011011101000001001100
10000000000000000000000000010000
10100000111000000000000000011000
10000000111000000000000000011000
01000000000010010000000000000000
11100001000000000000000000010100
10000000111000000000000000011000
11010001000011100000101001001100
01000000000010000000000000011100
10000001111000000000000000000100
10010010000000000000000000000000
01000000000010000000000000000001
10010011000100000000000000000000
01000001000110000000000000000001
00000100001010010000000000000011
11101001000000000000000000000010
01000100001010010000000000000000
11101001111111111111111111111000
11100001000000000000000000000111
10000000111000000000000000011000
10100000111000000000000000011100
10000000111000000000000000011100
11010001000011100011001101001100
10000000000000000000000000010000
10100000111000000000000000011000
11100111111111111111111111101001
10000000111000000000000000011000
01000000000010010000000000000000
11101001000000000000000001000010
01000000111010000000000000010100
10001101000000000000000000100010
01000001110110000000000000000000
11010111000000000000000000001100
10000000111000000000000000010100
11010001000011101000000001001100
01000001000000000000000000001000
10100001000000000000000000000000
10000000111000000000000000010100
11010001000011101001000001001100
01000001000000000000000000000000
10110001000000000000000000001101
10000000111000000000000000010100
11010001000011101010100101001100
01000000000010000000000000011100
10000001111000000000000000000100
01000010000000000000000000000110
10000011000100000000000000000000
01000001000110000000000000000100
10100011000000000000000000000000
01000000000010000000000000000100
01000010001010010000000000000001
11101001111111111111111111111010
10000000111000000000000000010100
11010001000011101011111001001100
01000000000010000000000000111000
10000001111000000000000000001000
01000010000000000000000000000110
10000011000100000000000000000000
01000001000110000000000000000100
10100011000000000000000000000000
01000000000010000000000000000100
01000010001010010000000000000001
11101001111111111111111111111010
10000000111000000000000000010100
11010001000011101101001001001100
10000001111000000000000000010000
10100001000000000000000000110100
10000000111000000000000000010100
11010001000011101110100001001100
10001101000000000000000000100111
10000001110100000000000000110000
10100001000000000000000000000100
01000000110110000000000000110000
10000001000000000000000000000000
01000001000110000000000000000001
10100001000000000000000000000000
10000000111000000000000000010100
11010001000011110000100101001100
10000001110100000000000000101000
10100001000000000000000000011000
10000000111000000000000000010100
11010001000011110001110001001100
01000001000000000000000000000000
10100001000000000000000000010100
10000000111000000000000000010100
11010001000011110010110101001100
01000001000000000000000000000000
10100001000000000000000000010000
10000000111000000000000000011100
11010001000011110100010101001100
10000001111000000000000000010100
10100001000000000000000000010000
10000000111000000000000000010100
10100000111000000000000000011000
11100111000000000000000000000101
10010000111000000000000000001100
01000000000010010000000000000000
11100001000000000000000000000010
10000000111000000000000000010100
10100000111000000000000000011000
10000000111000000000000000011000
10001111111000000000000000000000
01001110111010000000000000100000
11000111000000000000000000001111
01001110111010010000000000000100
10101111111000000000000000000000
10001101000000000000000000100101
10000000110100000000000000000100
10100000110100000000000000000000
01000000000000000000000000000001
10100000110100000000000000110000
10001111111000000000000000000000
01001110111010000000000000000100
11000111000000000000000000001111
01001110111010010000000000010100
10101111111000000000000000000000
10100000111000000000000000000100
10100001111000000000000000001000
10100010111000000000000000001100
01000000111010000000000000010000
10001101000000000000000000001110
01000001110110000000000000000000
11010111000000000000000000001100
10000000111000000000000000010000
11010001000100000111110101001100
10000001111000000000000000001000
10100001000000000000000000000000
10000000111000000000000000010000
11010001000100001000111001001100
10000001111000000000000000001100
10100001000000000000000000100000
10000000111000000000000000010000
11010001000100001001111001001100
10000001111000000000000000000100
10100001000000000000000000000100
10000000111000000000000000010000
11010001000100001010111001001100
01000001000000000000000000000000
10100001000000000000000000011100
10000000111000000000000000000100
01000001000010010000000001000000
11011010000100001100011000011100
01000000000000010000000000000010
10001101000000000000000000010111
00000000110110000000000000000000
10000001111000000000000000010000
10100001000000000000000000111000
10000000111000000000000000010000
10001111111000000000000000000000
01001110111010000000000000010100
11000111000000000000000000001111
01001110111010010000000000011100
10101111111000000000000000000000
10100000111000000000000000000100
10100001111000000000000000001000
10100010111000000000000000001100
10100011111000000000000000010000
10100100111000000000000000010100
01000000111010000000000000011000
10001101000000000000000000010000
01000001110110000000000000000000
11010111000000000000000000001100
10000000111000000000000000011000
11010001000100010110000101001100
01000000000010000000000000011100
10000001111000000000000000000100
10000010111000000000000000001000
11100001000000000000000000001011
01000010001010000000000000000011
01000010001000100000000000000010
01000011000000000000000000000110
00000011001010010000000000000011
11011110000100010110101000111100
10000011000100000000000000000000
01000001000110000000000000000100
10100011000000000000000000000000
01000000000010000000000000000100
01000010001010010000000000000001
11101001111111111111111111111010
10000000111000000000000000011000
11010001000100010111010001001100
10000001111000000000000000001100
10100001000000000000000000000000
10000000111000000000000000011000
11010001000100011000010001001100
10000001111000000000000000010000
10100001000000000000000000011000
10000000111000000000000000011000
11010001000100011001010101001100
10000001111000000000000000010100
10100001000000000000000000110100
10000000111000000000000000011000
11010001000100011010001101001100
01000001000000000000000000000000
10100001000000000000000000010100
10000000111000000000000000001100
01000000000010010000000000000101
11101001000000000000000000000100
10000000111000000000000000010000
11010001000100011100110001001100
10000001111000000000000000011000
10100001000000000000000000011000
10000000111000000000000000011000
11010001000100011110011001001100
10001101000000000000000000101101
10000001110100000000000000001000
10100001000000000000000000010000
10000000111000000000000000011000
10100000110100000000000000001000
10001111111000000000000000000000
01001110111010000000000000011100
11000111000000000000000000001111
01001110111010010000000000000100
10101111111000000000000000000000
01000000000000000000000000000001
01000001000000000000000000000100
01000010000000000000000000000001
11110111111111111111111110011000
10001101000000000000000000001110
10100000110100000000000000001100
01000000000000000000000000000010
01000001000000000000000000000010
01000010000000000000000000000001
11110111111111111111111110010010
10001101000000000000000000000110
10100000110100000000000000010000
01000000000000000000000000000011
01000001000000000000000000000011
01000010000000000000000000000001
11110111111111111111111110001100
10001101000000000000000000000110
10100000110100000000000000010100
01000000000000000000000000000100
01000001000000000000000000000100
01000010000000000000000000000100
11110111111111111111111110000110
10001101000000000000000000000110
10100000110100000000000000011000
01000000000000000000000000000101
01000001000000000000000000000101
01000010000000000000000000000100
11110111111111111111111110000000
10001101000000000000000000000110
10100000110100000000000000011100
01000000000000000000000000000110
01000001000000000000000000000110
01000010000000000000000000000100
11110111111111111111111101111010
10001101000000000000000000000110
10100000110100000000000000100000
01000000000000000000000000001000
01000001000000000000000000001000
01000010000000000000000000000100
11110111111111111111111101110100
10001101000000000000000000000110
10100000110100000000000000100100
01000000000000000000000000001001
01000001000000000000000000001001
01000010000000000000000000000100
11110111111111111111111101101110
10001101000000000000000000000110
10100000110100000000000000101000
01000000000000000000000000001011
01000001000000000000000000001011
01000010000000000000000000001000
11110111111111111111111101101000
10001101000000000000000000000110
10100000110100000000000000101100
01000000000000000000000000000000
10100000110100000000000000001000
01000000110110000000000100111000
01000001000000000000000000000100
01000010000000000000000000000111
10000011110100000000000000011000
01000100000000000000000010000100
11110111111111111111111110000011
10001101000000000000000000001010
01000000110110000000000100111100
01000001000000000000000000000100
01000010000000000000000000000111
10000011110100000000000000011000
01000100000000000000000001111010
11110111111111111111111101111100
10001101000000000000000000000111
01000000110110000000000101000000
01000001000000000000000000000100
01000010000000000000000000000111
10000011110100000000000000011000
01000100000000000000000001110000
11110111111111111111111101110101
10001101000000000000000000000111
01000000110110000000000101000100
01000001000000000000000000000100
01000010000000000000000000000111
10000011110100000000000000011000
01000100000000000000000001011100
11110111111111111111111101101110
10001101000000000000000000000111
01000000110110000000000101001000
01000001000000000000000000000100
01000010000000000000000000000111
10000011110100000000000000011000
01000100000000000000000001010010
11110111111111111111111101100111
10001101000000000000000000000111
01000000110110000000000101001100
01000001000000000000000000000100
01000010000000000000000000000111
10000011110100000000000000011000
01000100000000000000000001001000
11110111111111111111111101100000
10001101000000000000000000000111
01000000110110000000000101010000
01000001000000000000000000000100
01000010000000000000000000000111
10000011110100000000000000011000
01000100000000000000000000111101
11110111111111111111111101011001
10001101000000000000000000000111
01000000110110000000000101010100
01000001000000000000000000000100
01000010000000000000000000000111
10000011110100000000000000010100
01000100000000000000000000110011
11110111111111111111111101010010
10001101000000000000000000000111
01000000110110000000000101011000
01000001000000000000000000000100
01000010000000000000000000000111
10000011110100000000000000011000
01000100000000000000000000101001
11110111111111111111111101001011
10001101000000000000000000000111
01000000110110000000000101011100
01000001000000000000000000000100
01000010000000000000000000000111
10000011110100000000000000011100
01000100000000000000000000011111
11110111111111111111111101000100
10001101000000000000000000000111
01000000110110000000000101100000
01000001000000000000000000000110
01000010000000000000000000000111
10000011110100000000000000011000
01000100000000000000000000010101
11110111111111111111111100111101
10001101000000000000000000000111
01000000110110000000000101101000
01000001000000000000000000000100
01000010000000000000000000000111
10000011110100000000000000010000
01000100000000000000000000001011
11110111111111111111111100110110
10001101000000000000000000000111
01000000110110000000000101101100
01000001000000000000000000000100
01000010000000000000000000000111
10000011110100000000000000011000
01000100000000000000000000000001
11110111111111111111111100101111
10001101000000000000000000000111
01000000110110000000000101110000
01000001000000000000000000000100
01000010000000000000000000000110
10000011110100000000000000101000
01000100000000000000000001010001
11110111111111111111111100101000
10001101000000000000000000000111
01000000110110000000000101110100
01000001000000000000000000000101
01000010000000000000000000000110
10000011110100000000000000101000
01000100000000000000000001001000
11110111111111111111111100100001
10001101000000000000000000000111
01000000110110000000000101111100
01000001000000000000000000000101
01000010000000000000000000000110
10000011110100000000000000101000
01000100000000000000000000111110
11110111111111111111111100011010
10001101000000000000000000000111
01000000110110000000000110000100
01000001000000000000000000000100
01000010000000000000000000000110
10000011110100000000000000101000
01000100000000000000000000110011
11110111111111111111111100010011
10001101000000000000000000000111
01000000110110000000000110001000
01000001000000000000000000000111
01000010000000000000000000000110
10000011110100000000000000101000
01000100000000000000000000101001
11110111111111111111111100001100
10001101000000000000000000000111
01000000110110000000000110010000
01000001000000000000000000000101
01000010000000000000000000000110
10000011110100000000000000101000
01000100000000000000000000100000
11110111111111111111111100000101
10001101000000000000000000000111
01000000110110000000000110011000
01000001000000000000000000000101
01000010000000000000000000000110
10000011110100000000000000101000
01000100000000000000000000010110
11110111111111111111111011111110
10001101000000000000000000000111
01000000110110000000000110100000
01000001000000000000000000000100
01000010000000000000000000000110
10000011110100000000000000101000
01000100000000000000000000001011
11110111111111111111111011110111
10001101000000000000000000000111
01000000110110000000000110100100
01000001000000000000000000000100
01000010000000000000000000000110
10000011110100000000000000101000
01000100000000000000000000000001
11110111111111111111111011110000
10001101000000000000000000000111
01000000110110000000000110101000
01000001000000000000000000000100
01000010000000000000000000000101
10000011110100000000000000100000
01000100000000000000000000000000
11110111111111111111111011101001
10001101000000000000000000000111
01000000110110000000000110101100
01000001000000000000000000001000
01000010000000000000000000000101
10000011110100000000000000010000
01000100000000000000000000000000
11110111111111111111111011100010
10001101000000000000000000000111
01000000110110000000000110110100
01000001000000000000000000000101
01000010000000000000000000000101
10000011110100000000000000001100
01000100000000000000000000000000
11110111111111111111111011011011
10001101000000000000000000000111
01000000110110000000000110111100
01000001000000000000000000000101
01000010000000000000000000000101
10000011110100000000000000010100
01000100000000000000000000000000
11110111111111111111111011010100
10001101000000000000000000000111
01000000110110000000000111000100
01000001000000000000000000001001
01000010000000000000000000000101
10000011110100000000000000011100
01000100000000000000000000000000
11110111111111111111111011001101
10001101000000000000000000000111
01000000110110000000000111010000
01000001000000000000000000000101
01000010000000000000000000000101
10000011110100000000000000011100
01000100000000000000000000000000
11110111111111111111111011000110
10001101000000000000000000000111
01000000110110000000000111011000
01000001000000000000000000001000
01000010000000000000000000000101
10000011110100000000000000011000
01000100000000000000000000000000
11110111111111111111111010111111
10001101000000000000000000000111
01000000110110000000000111100000
01000001000000000000000000001000
01000010000000000000000000000101
10000011110100000000000000011000
01000100000000000000000000000000
11110111111111111111111010111000
01000000000000000000000000000000
10001101000000000000000000001000
10100000110100000000000000000000
11110111111111111111110110100011
10001101000000000000000000000011
10000000110100000000000000000000
11010001000110001101100101001100
10000001110100000000000000001000
10100001000000000000000000010000
10000000110100000000000000000000
10100000110100000000000000000100
01000000000000000000000000000000
10100000110100000000000000001000
01000000110110000000000111101000
01000001000000000000000000000010
01000010000000000000000000000111
10000011110100000000000000011000
01000100000000000000000011001001
11110111111111111111111010100101
10001101000000000000000000001111
01000000110110000000000111101100
01000001000000000000000000000101
01000010000000000000000000000111
10000011110100000000000000010000
01000100000000000000000010111111
11110111111111111111111010011110
10001101000000000000000000000111
01000000110110000000000111110100
01000001000000000000000000000101
01000010000000000000000000000111
10000011110100000000000000011000
01000100000000000000000010110101
11110111111111111111111010010111
10001101000000000000000000000111
01000000110110000000000111111100
01000001000000000000000000000100
01000010000000000000000000000111
10000011110100000000000000011000
01000100000000000000000010101011
11110111111111111111111010010000
10001101000000000000000000000111
01000000110110000000001000000000
01000001000000000000000000000100
01000010000000000000000000000111
10000011110100000000000000011000
01000100000000000000000010100010
11110111111111111111111010001001
10001101000000000000000000000111
01000000110110000000001000000100
01000001000000000000000000000100
01000010000000000000000000000111
10000011110100000000000000011000
01000100000000000000000010010111
11110111111111111111111010000010
10001101000000000000000000000111
01000000110110000000001000001000
01000001000000000000000000000100
01000010000000000000000000000111
10000011110100000000000000010000
01000100000000000000000010001110
11110111111111111111111001111011
10001101000000000000000000000111
01000000110110000000001000001100
01000001000000000000000000000110
01000010000000000000000000000110
10000011110100000000000000101000
01000100000000000000000010001110
11110111111111111111111001110100
10001101000000000000000000000111
01000000110110000000001000010100
01000001000000000000000000000110
01000010000000000000000000000110
10000011110100000000000000101000
01000100000000000000000010000011
11110111111111111111111001101101
10001101000000000000000000000111
01000000110110000000001000011100
01000001000000000000000000000101
01000010000000000000000000000110
10000011110100000000000000101000
01000100000000000000000001111011
11110111111111111111111001100110
10001101000000000000000000000111
01000000110110000000001000100100
01000001000000000000000000000100
01000010000000000000000000000110
10000011110100000000000000101000
01000100000000000000000001110000
11110111111111111111111001011111
10001101000000000000000000000111
01000000110110000000001000101000
01000001000000000000000000000100
01000010000000000000000000000110
10000011110100000000000000101000
01000100000000000000000001100110
11110111111111111111111001011000
10001111111000000000000000000000
01001110111010000000000000000100
11000111000000000000000000001111'''.split()]


if __name__ == '__main__':
  for i, n in enumerate(data):
  ##  print '0x%08x' % i, dis(n)
    print dis(n)
