package cpu6502

import (
	"reflect"
	"runtime"
)

type instruction struct {
	name        string
	execute     func()
	addrMode    func() bool
	cycles      uint64
	extraCycles uint64
}

func nameof(i interface{}) string {
	return runtime.FuncForPC(reflect.ValueOf(i).Pointer()).Name()
}

func newInstruction(n string, p func(), a func() bool, c uint64, e uint64) *instruction {
	return &instruction{n, p, a, c, e}
}

type CPU struct {
	cycle uint64

	// Registers
	PC uint16
	SP byte
	A  byte
	X  byte
	Y  byte

	// Status
	C byte // carry
	Z byte // zero
	I byte // disable interrupt
	D byte // decimal mode
	B byte // break
	U byte // unused
	V byte // overflow
	N byte // negative

	opcode        byte
	effectiveAddr uint16
	instructions  [256]instruction

	read  func(uint16) byte
	write func(uint16, byte)
}

func NewCPU(r func(uint16) byte, w func(uint16, byte)) *CPU {
	cpu := CPU{read: r, write: w}
	cpu.instructions = [256]instruction{
		{"brk", cpu.brk, cpu.immediate, 7, 0},
		{"ora", cpu.ora, cpu.indexedindirect, 6, 0},
		{"kil", cpu.illegal, cpu.implied, 2, 0},
		{"slo", cpu.illegal, cpu.implied, 8, 0},
		{"nop", cpu.nop, cpu.implied, 3, 0},
		{"ora", cpu.ora, cpu.zeropage, 3, 0},
		{"asl", cpu.asl, cpu.zeropage, 5, 0},
		{"slo", cpu.illegal, cpu.implied, 5, 0},
		{"php", cpu.php, cpu.implied, 3, 0},
		{"ora", cpu.ora, cpu.immediate, 2, 0},
		{"asl", cpu.asl, cpu.accumulator, 2, 0},
		{"anc", cpu.illegal, cpu.implied, 2, 0},
		{"nop", cpu.nop, cpu.implied, 4, 0},
		{"ora", cpu.ora, cpu.absolute, 4, 0},
		{"asl", cpu.asl, cpu.absolute, 6, 0},
		{"slo", cpu.illegal, cpu.implied, 6, 0},
		{"bpl", cpu.bpl, cpu.relative, 2, 1},
		{"ora", cpu.ora, cpu.indirectindexed, 5, 1},
		{"kil", cpu.illegal, cpu.implied, 2, 0},
		{"slo", cpu.illegal, cpu.implied, 8, 0},
		{"nop", cpu.nop, cpu.implied, 4, 0},
		{"ora", cpu.ora, cpu.zeropagex, 4, 0},
		{"asl", cpu.asl, cpu.zeropagex, 6, 0},
		{"slo", cpu.illegal, cpu.implied, 6, 0},
		{"clc", cpu.clc, cpu.implied, 2, 0},
		{"ora", cpu.ora, cpu.absolutey, 4, 1},
		{"nop", cpu.nop, cpu.implied, 2, 0},
		{"slo", cpu.illegal, cpu.implied, 7, 0},
		{"nop", cpu.nop, cpu.implied, 4, 1},
		{"ora", cpu.ora, cpu.absolutex, 4, 1},
		{"asl", cpu.asl, cpu.absolutex, 7, 0},
		{"slo", cpu.illegal, cpu.implied, 7, 0},
		{"jsr", cpu.jsr, cpu.absolute, 6, 0},
		{"and", cpu.and, cpu.indexedindirect, 6, 0},
		{"kil", cpu.illegal, cpu.implied, 2, 0},
		{"rla", cpu.illegal, cpu.implied, 8, 0},
		{"bit", cpu.bit, cpu.zeropage, 3, 0},
		{"and", cpu.and, cpu.zeropage, 3, 0},
		{"rol", cpu.rol, cpu.zeropage, 5, 0},
		{"rla", cpu.illegal, cpu.implied, 5, 0},
		{"plp", cpu.plp, cpu.implied, 4, 0},
		{"and", cpu.and, cpu.immediate, 2, 0},
		{"rol", cpu.rol, cpu.accumulator, 2, 0},
		{"anc", cpu.illegal, cpu.implied, 2, 0},
		{"bit", cpu.bit, cpu.absolute, 4, 0},
		{"and", cpu.and, cpu.absolute, 4, 0},
		{"rol", cpu.rol, cpu.absolute, 6, 0},
		{"rla", cpu.illegal, cpu.implied, 6, 0},
		{"bmi", cpu.bmi, cpu.relative, 2, 1},
		{"and", cpu.and, cpu.indirectindexed, 5, 1},
		{"kil", cpu.illegal, cpu.implied, 2, 0},
		{"rla", cpu.illegal, cpu.implied, 8, 0},
		{"nop", cpu.nop, cpu.implied, 4, 0},
		{"and", cpu.and, cpu.zeropagex, 4, 0},
		{"rol", cpu.rol, cpu.zeropagex, 6, 0},
		{"rla", cpu.illegal, cpu.implied, 6, 0},
		{"sec", cpu.sec, cpu.implied, 2, 0},
		{"and", cpu.and, cpu.absolutey, 4, 1},
		{"nop", cpu.nop, cpu.implied, 2, 0},
		{"rla", cpu.illegal, cpu.implied, 7, 0},
		{"nop", cpu.nop, cpu.implied, 4, 1},
		{"and", cpu.and, cpu.absolutex, 4, 1},
		{"rol", cpu.rol, cpu.absolutex, 7, 0},
		{"rla", cpu.illegal, cpu.implied, 7, 0},
		{"rti", cpu.rti, cpu.implied, 6, 0},
		{"eor", cpu.eor, cpu.indexedindirect, 6, 0},
		{"kil", cpu.illegal, cpu.implied, 2, 0},
		{"sre", cpu.illegal, cpu.implied, 8, 0},
		{"nop", cpu.nop, cpu.implied, 3, 0},
		{"eor", cpu.eor, cpu.zeropage, 3, 0},
		{"lsr", cpu.lsr, cpu.zeropage, 5, 0},
		{"sre", cpu.illegal, cpu.implied, 5, 0},
		{"pha", cpu.pha, cpu.implied, 3, 0},
		{"eor", cpu.eor, cpu.immediate, 2, 0},
		{"lsr", cpu.lsr, cpu.accumulator, 2, 0},
		{"alr", cpu.illegal, cpu.implied, 2, 0},
		{"jmp", cpu.jmp, cpu.absolute, 3, 0},
		{"eor", cpu.eor, cpu.absolute, 4, 0},
		{"lsr", cpu.lsr, cpu.absolute, 6, 0},
		{"sre", cpu.illegal, cpu.implied, 6, 0},
		{"bvc", cpu.bvc, cpu.relative, 2, 1},
		{"eor", cpu.eor, cpu.indirectindexed, 5, 1},
		{"kil", cpu.illegal, cpu.implied, 2, 0},
		{"sre", cpu.illegal, cpu.implied, 8, 0},
		{"nop", cpu.nop, cpu.implied, 4, 0},
		{"eor", cpu.eor, cpu.zeropagex, 4, 0},
		{"lsr", cpu.lsr, cpu.zeropagex, 6, 0},
		{"sre", cpu.illegal, cpu.implied, 6, 0},
		{"cli", cpu.cli, cpu.implied, 2, 0},
		{"eor", cpu.eor, cpu.absolutey, 4, 1},
		{"nop", cpu.nop, cpu.implied, 2, 0},
		{"sre", cpu.illegal, cpu.implied, 7, 0},
		{"nop", cpu.nop, cpu.implied, 4, 1},
		{"eor", cpu.eor, cpu.absolutex, 4, 1},
		{"lsr", cpu.lsr, cpu.absolutex, 7, 0},
		{"sre", cpu.illegal, cpu.implied, 7, 0},
		{"rts", cpu.rts, cpu.implied, 6, 0},
		{"adc", cpu.adc, cpu.indexedindirect, 6, 0},
		{"kil", cpu.illegal, cpu.implied, 2, 0},
		{"rra", cpu.illegal, cpu.implied, 8, 0},
		{"nop", cpu.nop, cpu.implied, 3, 0},
		{"adc", cpu.adc, cpu.zeropage, 3, 0},
		{"ror", cpu.ror, cpu.zeropage, 5, 0},
		{"rra", cpu.illegal, cpu.implied, 5, 0},
		{"pla", cpu.pla, cpu.implied, 4, 0},
		{"adc", cpu.adc, cpu.immediate, 2, 0},
		{"ror", cpu.ror, cpu.accumulator, 2, 0},
		{"arr", cpu.illegal, cpu.implied, 2, 0},
		{"jmp", cpu.jmp, cpu.indirect, 5, 0},
		{"adc", cpu.adc, cpu.absolute, 4, 0},
		{"ror", cpu.ror, cpu.absolute, 6, 0},
		{"rra", cpu.illegal, cpu.implied, 6, 0},
		{"bvs", cpu.bvs, cpu.relative, 2, 1},
		{"adc", cpu.adc, cpu.indirectindexed, 5, 1},
		{"kil", cpu.illegal, cpu.implied, 2, 0},
		{"rra", cpu.illegal, cpu.implied, 8, 0},
		{"nop", cpu.nop, cpu.implied, 4, 0},
		{"adc", cpu.adc, cpu.zeropagex, 4, 0},
		{"ror", cpu.ror, cpu.zeropagex, 6, 0},
		{"rra", cpu.illegal, cpu.implied, 6, 0},
		{"sei", cpu.sei, cpu.implied, 2, 0},
		{"adc", cpu.adc, cpu.absolutey, 4, 1},
		{"nop", cpu.nop, cpu.implied, 2, 0},
		{"rra", cpu.illegal, cpu.implied, 7, 0},
		{"nop", cpu.nop, cpu.implied, 4, 1},
		{"adc", cpu.adc, cpu.absolutex, 4, 1},
		{"ror", cpu.ror, cpu.absolutex, 7, 0},
		{"rra", cpu.illegal, cpu.implied, 7, 0},
		{"nop", cpu.nop, cpu.implied, 2, 0},
		{"sta", cpu.sta, cpu.indexedindirect, 6, 0},
		{"nop", cpu.nop, cpu.implied, 2, 0},
		{"sax", cpu.illegal, cpu.implied, 6, 0},
		{"sty", cpu.sty, cpu.zeropage, 3, 0},
		{"sta", cpu.sta, cpu.zeropage, 3, 0},
		{"stx", cpu.stx, cpu.zeropage, 3, 0},
		{"sax", cpu.illegal, cpu.implied, 3, 0},
		{"dey", cpu.dey, cpu.implied, 2, 0},
		{"nop", cpu.nop, cpu.implied, 2, 0},
		{"txa", cpu.txa, cpu.implied, 2, 0},
		{"XAA", cpu.illegal, cpu.implied, 2, 0},
		{"sty", cpu.sty, cpu.absolute, 4, 0},
		{"sta", cpu.sta, cpu.absolute, 4, 0},
		{"stx", cpu.stx, cpu.absolute, 4, 0},
		{"sax", cpu.illegal, cpu.implied, 4, 0},
		{"bcc", cpu.bcc, cpu.relative, 2, 1},
		{"sta", cpu.sta, cpu.indirectindexed, 6, 0},
		{"kil", cpu.illegal, cpu.implied, 2, 0},
		{"ahx", cpu.illegal, cpu.implied, 6, 0},
		{"sty", cpu.sty, cpu.zeropagex, 4, 0},
		{"sta", cpu.sta, cpu.zeropagex, 4, 0},
		{"stx", cpu.stx, cpu.zeropagey, 4, 0},
		{"sax", cpu.illegal, cpu.implied, 4, 0},
		{"tya", cpu.tya, cpu.implied, 2, 0},
		{"sta", cpu.sta, cpu.absolutey, 5, 0},
		{"txs", cpu.txs, cpu.implied, 2, 0},
		{"tas", cpu.illegal, cpu.implied, 5, 0},
		{"shy", cpu.nop, cpu.implied, 5, 0},
		{"sta", cpu.sta, cpu.absolutex, 5, 0},
		{"shx", cpu.illegal, cpu.implied, 5, 0},
		{"ahx", cpu.illegal, cpu.implied, 5, 0},
		{"ldy", cpu.ldy, cpu.immediate, 2, 0},
		{"lda", cpu.lda, cpu.indexedindirect, 6, 0},
		{"ldx", cpu.ldx, cpu.immediate, 2, 0},
		{"lax", cpu.illegal, cpu.implied, 6, 0},
		{"ldy", cpu.ldy, cpu.zeropage, 3, 0},
		{"lda", cpu.lda, cpu.zeropage, 3, 0},
		{"ldx", cpu.ldx, cpu.zeropage, 3, 0},
		{"lax", cpu.illegal, cpu.implied, 3, 0},
		{"tay", cpu.tay, cpu.implied, 2, 0},
		{"lda", cpu.lda, cpu.immediate, 2, 0},
		{"tax", cpu.tax, cpu.implied, 2, 0},
		{"lax", cpu.illegal, cpu.implied, 2, 0},
		{"ldy", cpu.ldy, cpu.absolute, 4, 0},
		{"lda", cpu.lda, cpu.absolute, 4, 0},
		{"ldx", cpu.ldx, cpu.absolute, 4, 0},
		{"lax", cpu.illegal, cpu.implied, 4, 0},
		{"bcs", cpu.bcs, cpu.relative, 2, 1},
		{"lda", cpu.lda, cpu.indirectindexed, 5, 1},
		{"kil", cpu.illegal, cpu.implied, 2, 0},
		{"lax", cpu.illegal, cpu.implied, 5, 1},
		{"ldy", cpu.ldy, cpu.zeropagex, 4, 0},
		{"lda", cpu.lda, cpu.zeropagex, 4, 0},
		{"ldx", cpu.ldx, cpu.zeropagey, 4, 0},
		{"lax", cpu.illegal, cpu.implied, 4, 0},
		{"clv", cpu.clv, cpu.implied, 2, 0},
		{"lda", cpu.lda, cpu.absolutey, 4, 1},
		{"tsx", cpu.tsx, cpu.implied, 2, 0},
		{"las", cpu.illegal, cpu.implied, 4, 1},
		{"ldy", cpu.ldy, cpu.absolutex, 4, 1},
		{"lda", cpu.lda, cpu.absolutex, 4, 1},
		{"ldx", cpu.ldx, cpu.absolutey, 4, 1},
		{"lax", cpu.illegal, cpu.implied, 4, 1},
		{"cpy", cpu.cpy, cpu.immediate, 2, 0},
		{"cmp", cpu.cmp, cpu.indexedindirect, 6, 0},
		{"nop", cpu.nop, cpu.implied, 2, 0},
		{"dcp", cpu.illegal, cpu.implied, 8, 0},
		{"cpy", cpu.cpy, cpu.zeropage, 3, 0},
		{"cmp", cpu.cmp, cpu.zeropage, 3, 0},
		{"dec", cpu.dec, cpu.zeropage, 5, 0},
		{"dcp", cpu.illegal, cpu.implied, 5, 0},
		{"iny", cpu.iny, cpu.implied, 2, 0},
		{"cmp", cpu.cmp, cpu.immediate, 2, 0},
		{"dex", cpu.dex, cpu.implied, 2, 0},
		{"axs", cpu.illegal, cpu.implied, 2, 0},
		{"cpy", cpu.cpy, cpu.absolute, 4, 0},
		{"cmp", cpu.cmp, cpu.absolute, 4, 0},
		{"dec", cpu.dec, cpu.absolute, 6, 0},
		{"dcp", cpu.illegal, cpu.implied, 6, 0},
		{"bne", cpu.bne, cpu.relative, 2, 1},
		{"cmp", cpu.cmp, cpu.indirectindexed, 5, 1},
		{"kil", cpu.illegal, cpu.implied, 2, 0},
		{"dcp", cpu.illegal, cpu.implied, 8, 0},
		{"nop", cpu.nop, cpu.implied, 4, 0},
		{"cmp", cpu.cmp, cpu.zeropagex, 4, 0},
		{"dec", cpu.dec, cpu.zeropagex, 6, 0},
		{"dcp", cpu.illegal, cpu.implied, 6, 0},
		{"cld", cpu.cld, cpu.implied, 2, 0},
		{"cmp", cpu.cmp, cpu.absolutey, 4, 1},
		{"nop", cpu.nop, cpu.implied, 2, 0},
		{"dcp", cpu.illegal, cpu.implied, 7, 0},
		{"nop", cpu.nop, cpu.implied, 4, 1},
		{"cmp", cpu.cmp, cpu.absolutex, 4, 1},
		{"dec", cpu.dec, cpu.absolutex, 7, 0},
		{"dcp", cpu.illegal, cpu.implied, 7, 0},
		{"cpx", cpu.cpx, cpu.immediate, 2, 0},
		{"sbc", cpu.sbc, cpu.indexedindirect, 6, 0},
		{"nop", cpu.nop, cpu.implied, 2, 0},
		{"isc", cpu.illegal, cpu.implied, 8, 0},
		{"cpx", cpu.cpx, cpu.zeropage, 3, 0},
		{"sbc", cpu.sbc, cpu.zeropage, 3, 0},
		{"inc", cpu.inc, cpu.zeropage, 5, 0},
		{"isc", cpu.illegal, cpu.implied, 5, 0},
		{"inx", cpu.inx, cpu.implied, 2, 0},
		{"sbc", cpu.sbc, cpu.immediate, 2, 0},
		{"nop", cpu.nop, cpu.implied, 2, 0},
		{"sbc", cpu.sbc, cpu.implied, 2, 0},
		{"cpx", cpu.cpx, cpu.absolute, 4, 0},
		{"sbc", cpu.sbc, cpu.absolute, 4, 0},
		{"inc", cpu.inc, cpu.absolute, 6, 0},
		{"isc", cpu.illegal, cpu.implied, 6, 0},
		{"beq", cpu.beq, cpu.relative, 2, 1},
		{"sbc", cpu.sbc, cpu.indirectindexed, 5, 1},
		{"kil", cpu.illegal, cpu.implied, 2, 0},
		{"isc", cpu.illegal, cpu.implied, 8, 0},
		{"nop", cpu.nop, cpu.implied, 4, 0},
		{"sbc", cpu.sbc, cpu.zeropagex, 4, 0},
		{"inc", cpu.inc, cpu.zeropagex, 6, 0},
		{"isc", cpu.illegal, cpu.implied, 6, 0},
		{"sed", cpu.sed, cpu.implied, 2, 0},
		{"sbc", cpu.sbc, cpu.absolutey, 4, 1},
		{"nop", cpu.nop, cpu.implied, 2, 0},
		{"isc", cpu.illegal, cpu.implied, 7, 0},
		{"nop", cpu.nop, cpu.implied, 4, 1},
		{"sbc", cpu.sbc, cpu.absolutex, 4, 1},
		{"inc", cpu.inc, cpu.absolutex, 7, 0},
		{"isc", cpu.illegal, cpu.implied, 7, 0},
	}
	cpu.PC = cpu.read16(0xFFFC)
	cpu.SP = 0xFD
	cpu.I = 1
	cpu.U = 1
	return &cpu
}

func (cpu *CPU) Status() byte {
	var s byte
	s |= cpu.C << 0
	s |= cpu.Z << 1
	s |= cpu.I << 2
	s |= cpu.D << 3
	s |= cpu.B << 4
	s |= cpu.U << 5
	s |= cpu.V << 6
	s |= cpu.N << 7
	return s
}

func (cpu *CPU) setStatus(v byte) {
	cpu.C = (v >> 0) & 1
	cpu.Z = (v >> 1) & 1
	cpu.I = (v >> 2) & 1
	cpu.D = (v >> 3) & 1
	cpu.B = (v >> 4) & 1
	cpu.U = (v >> 5) & 1
	cpu.V = (v >> 6) & 1
	cpu.N = (v >> 7) & 1
}

func (c *CPU) setZN(v byte) {
	if v == 0 {
		c.Z = 1
	} else {
		c.Z = 0
	}
	if v&(1<<7) != 0 {
		c.N = 1
	} else {
		c.N = 0
	}
}

func (c *CPU) read16(addr uint16) uint16 {
	lo := uint16(c.read(addr))
	hi := uint16(c.read(addr + 1))
	return (hi << 8) | lo
}

func (c *CPU) push(v byte) {
	c.write(0x0100|uint16(c.SP), v)
	c.SP--
}

func (c *CPU) push16(v uint16) {
	c.push(byte(v >> 8))
	c.push(byte(v & 0xFF))
}

func (c *CPU) pop() byte {
	c.SP++
	return c.read(0x100 | uint16(c.SP))
}

func (c *CPU) irq() {
	if c.I == 0 {
		c.push16(c.PC)

		c.push(c.Status() | 0x10)
		c.I = 1

		c.PC = c.read16(0xFFFE)
		c.cycle += 7
	}
}

func (c *CPU) nmi() {
	c.push16(c.PC)

	c.push(c.Status() | 0x10)
	c.I = 1

	c.PC = c.read16(0xFFFA)
	c.cycle += 7
}

func (c *CPU) clock() {
	if c.cycle == 0 {
		c.opcode = c.read(c.PC)
		c.U = 1
		c.PC++
		c.cycle = c.instructions[c.opcode].cycles
		if c.instructions[c.opcode].addrMode() {
			c.cycle += c.instructions[c.opcode].extraCycles
		}
		c.instructions[c.opcode].execute()
		c.U = 1
	}
	c.cycle--
}

func (c *CPU) page_boundary_crossed(a uint16, b uint16) bool {
	return (a & 0xFF00) != (b & 0xFF00)
}

func (c *CPU) implied() bool {
	c.effectiveAddr = 0
	return false
}

func (c *CPU) accumulator() bool {
	c.effectiveAddr = 0
	return false
}

func (c *CPU) immediate() bool {
	c.effectiveAddr = c.PC
	c.PC++
	return false
}

func (c *CPU) zeropage() bool {
	c.effectiveAddr = uint16(c.read(c.PC)) & 0x00FF
	c.PC++
	return false
}

func (c *CPU) zeropagex() bool {
	c.effectiveAddr = uint16(c.read(c.PC)+c.X) & 0x00FF
	c.PC++
	return false
}

func (c *CPU) zeropagey() bool {
	c.effectiveAddr = uint16(c.read(c.PC)+c.Y) & 0x00FF
	c.PC++
	return false
}

func (c *CPU) relative() bool {
	offset := c.read(c.PC)
	c.PC++
	if offset < 0x80 {
		c.effectiveAddr = c.PC + uint16(offset)
	} else {
		c.effectiveAddr = c.PC + uint16(offset) - 256
	}
	return false
}

func (c *CPU) absolute() bool {
	c.effectiveAddr = c.read16(c.PC)
	c.PC += 2
	return false
}

func (c *CPU) absolutex() bool {
	c.effectiveAddr = c.read16(c.PC) + uint16(c.X)
	c.PC += 2
	return c.page_boundary_crossed(c.effectiveAddr, c.effectiveAddr-uint16(c.X))
}

func (c *CPU) absolutey() bool {
	c.effectiveAddr = c.read16(c.PC) + uint16(c.Y)
	c.PC += 2
	return c.page_boundary_crossed(c.effectiveAddr, c.effectiveAddr-uint16(c.Y))
}

func (c *CPU) indirect() bool {
	c.effectiveAddr = c.read16(c.PC)
	c.PC += 2
	if (c.effectiveAddr & 0x00FF) == 0x00FF {
		c.effectiveAddr =
			(uint16(c.read(c.effectiveAddr&0xFF00)) << 8) | uint16(c.read(c.effectiveAddr))
	} else {
		c.effectiveAddr = c.read16(c.effectiveAddr)
	}
	return false
}

func (c *CPU) indexedindirect() bool {
	c.effectiveAddr = uint16(c.read(c.PC))
	c.PC++
	lo := uint16(c.read((c.effectiveAddr + uint16(c.X)) & 0x00FF))
	hi := uint16(c.read((c.effectiveAddr + uint16(c.X) + 1) & 0x00FF))
	c.effectiveAddr = (hi << 8) | lo
	return false
}

func (c *CPU) indirectindexed() bool {
	c.effectiveAddr = uint16(c.read(c.PC))
	c.PC++

	lo := uint16(c.read(c.effectiveAddr & 0x00FF))
	hi := uint16(c.read((c.effectiveAddr + 1) & 0x00FF))

	c.effectiveAddr = (hi << 8) | lo
	c.effectiveAddr += uint16(c.Y)
	return c.page_boundary_crossed(c.effectiveAddr, c.effectiveAddr-uint16(c.Y))
}

func (c *CPU) adc() {
	a := c.A
	v := c.read(c.effectiveAddr)
	c.A = a + v + c.C
	c.setZN(c.A)
	if int(a)+int(v)+int(c.C) > 255 {
		c.C = 1
	} else {
		c.C = 0
	}
	if (a^v)&0x80 == 0 && (a^c.A)&0x80 != 0 {
		c.V = 1
	} else {
		c.V = 0
	}
}

func (c *CPU) and() {
	c.A &= c.read(c.effectiveAddr)
	c.setZN(c.A)
}

func (c *CPU) asl() {
	if nameof(c.instructions[c.opcode].addrMode) == nameof(c.accumulator) {
		c.C = (c.A >> 7) & 1
		c.A <<= 1
		c.setZN(c.A)
	} else {
		v := c.read(c.effectiveAddr)
		c.C = (v >> 7) & 1
		v <<= 1
		c.write(c.effectiveAddr, v)
		c.setZN(v)
	}
}

func (c *CPU) bcc() {
	if c.C == 0 {
		c.cycle++
		c.PC = c.effectiveAddr
	}
}

func (c *CPU) bcs() {
	if c.C != 0 {
		c.cycle++
		c.PC = c.effectiveAddr
	}
}

func (c *CPU) beq() {
	if c.Z != 0 {
		c.cycle++
		c.PC = c.effectiveAddr
	}
}

func (c *CPU) bit() {
	v := c.read(c.effectiveAddr)
	if (c.A & v) == 0 {
		c.Z = 1
	} else {
		c.Z = 0
	}
	c.V = (v >> 6) & 1
	if v&(1<<7) != 0 {
		c.N = 1
	} else {
		c.N = 0
	}

}

func (c *CPU) bmi() {
	if c.N != 0 {
		c.cycle++
		c.PC = c.effectiveAddr
	}
}

func (c *CPU) bne() {
	if c.Z == 0 {
		c.cycle++
		c.PC = c.effectiveAddr
	}
}

func (c *CPU) bpl() {
	if c.N == 0 {
		c.cycle++
		c.PC = c.effectiveAddr
	}
}

func (c *CPU) brk() {
	c.push16(c.PC)
	c.I = 1
	c.B = 1
	c.push(c.Status())
	c.B = 0
	c.read16(0xFFFE)
}

func (c *CPU) bvc() {
	if c.V == 0 {
		c.cycle++
		c.PC = c.effectiveAddr
	}
}

func (c *CPU) bvs() {
	if c.V != 0 {
		c.cycle++
		c.PC = c.effectiveAddr
	}
}

func (c *CPU) clc() { c.C = 0 }

func (c *CPU) cld() { c.D = 0 }

func (c *CPU) cli() { c.I = 0 }

func (c *CPU) clv() { c.V = 0 }

func (c *CPU) cmp() {
	v := c.read(c.effectiveAddr)
	if c.A >= v {
		c.C = 1
	} else {
		c.C = 0
	}
	c.setZN(c.A - v)
}

func (c *CPU) cpx() {
	v := c.read(c.effectiveAddr)
	if c.X >= v {
		c.C = 1
	} else {
		c.C = 0
	}
	c.setZN(c.X - v)
}

func (c *CPU) cpy() {
	v := c.read(c.effectiveAddr)
	if c.Y >= v {
		c.C = 1
	} else {
		c.C = 0
	}
	c.setZN(c.Y - v)
}

func (c *CPU) dec() {
	v := c.read(c.effectiveAddr) - 1
	c.write(c.effectiveAddr, v)
	c.setZN(v)
}

func (c *CPU) dex() {
	c.X--
	c.setZN(c.X)
}

func (c *CPU) dey() {
	c.Y--
	c.setZN(c.Y)
}

func (c *CPU) eor() {
	c.A ^= c.read(c.effectiveAddr)
	c.setZN(c.A)
}

func (c *CPU) inc() {
	v := c.read(c.effectiveAddr) + 1
	c.write(c.effectiveAddr, v)
	c.setZN(v)
}

func (c *CPU) inx() {
	c.X++
	c.setZN(c.X)
}

func (c *CPU) iny() {
	c.Y++
	c.setZN(c.Y)
}

func (c *CPU) jmp() {
	c.PC = c.effectiveAddr
}

func (c *CPU) jsr() {
	c.push16(c.PC - 1)
	c.PC = c.effectiveAddr
}

func (c *CPU) lda() {
	c.A = c.read(c.effectiveAddr)
	c.setZN(c.A)
}

func (c *CPU) ldx() {
	c.X = c.read(c.effectiveAddr)
	c.setZN(c.X)
}

func (c *CPU) ldy() {
	c.Y = c.read(c.effectiveAddr)
	c.setZN(c.Y)
}

func (c *CPU) lsr() {
	if nameof(c.instructions[c.opcode].addrMode) == nameof(c.accumulator) {
		c.C = c.A & 1
		c.A >>= 1
		c.setZN(c.A)
	} else {
		v := c.read(c.effectiveAddr)
		c.C = v & 1
		v >>= 1
		c.write(c.effectiveAddr, v)
		c.setZN(v)
	}
}

func (c *CPU) nop() {}

func (c *CPU) ora() {
	v := c.read(c.effectiveAddr)
	c.A |= v
	c.setZN(c.A)
}

func (c *CPU) pha() { c.push(c.A) }

func (c *CPU) php() {
	c.push(c.Status() | 0x10)
}

func (c *CPU) pla() {
	c.A = c.pop()
	c.setZN(c.A)
}

func (c *CPU) plp() {
	c.setStatus(c.pop())
	c.B = 0
	c.U = 1
}

func (c *CPU) rol() {
	if nameof(c.instructions[c.opcode].addrMode) == nameof(c.accumulator) {
		c.C = (c.A >> 7) & 1
		c.A = (c.A << 1) | c.C
		c.setZN(c.A)
	} else {
		x := c.C
		v := c.read(c.effectiveAddr)
		c.C = (v >> 7) & 1
		v = (v << 1) | x
		c.write(c.effectiveAddr, v)
		c.setZN(v)
	}
}

func (c *CPU) ror() {
	x := c.C
	if nameof(c.instructions[c.opcode].addrMode) == nameof(c.accumulator) {
		c.C = c.A & 1
		c.A = (c.A >> 1) | (x << 7)
		c.setZN(c.A)
	} else {
		v := c.read(c.effectiveAddr)
		c.C = v & 1
		v = (v >> 1) | (x << 7)
		c.write(c.effectiveAddr, v)
		c.setZN(v)
	}
}

func (c *CPU) rti() {
	c.setStatus(c.pop())
	c.B = 0
	c.U = 0
	c.PC = uint16(c.pop())
	c.PC = (uint16(c.pop()) << 8) | c.PC
}

func (c *CPU) rts() {
	c.PC = uint16(c.pop())
	c.PC = ((uint16(c.pop()) << 8) | c.PC) + 1
}

func (c *CPU) sbc() {
	a := c.A
	v := c.read(c.effectiveAddr) ^ 0xFF
	// same as adc
	c.A = a + v + c.C
	c.setZN(c.A)
	if int(a)+int(v)+int(c.C) > 255 {
		c.C = 1
	} else {
		c.C = 0
	}
	if (a^v)&0x80 == 0 && (a^c.A)&0x80 != 0 {
		c.V = 1
	} else {
		c.V = 0
	}
}

func (c *CPU) sec() { c.C = 1 }

func (c *CPU) sed() { c.D = 1 }

func (c *CPU) sei() { c.I = 1 }

func (c *CPU) sta() { c.write(c.effectiveAddr, c.A) }

func (c *CPU) stx() { c.write(c.effectiveAddr, c.X) }

func (c *CPU) sty() { c.write(c.effectiveAddr, c.Y) }

func (c *CPU) tax() {
	c.X = c.A
	c.setZN(c.X)
}

func (c *CPU) tay() {
	c.Y = c.A
	c.setZN(c.Y)
}

func (c *CPU) tsx() {
	c.X = c.SP
	c.setZN(c.X)
}

func (c *CPU) txa() {
	c.A = c.X
	c.setZN(c.A)
}

func (c *CPU) txs() { c.SP = c.X }

func (c *CPU) tya() {
	c.A = c.Y
	c.setZN(c.A)
}

func (c *CPU) illegal() {}
