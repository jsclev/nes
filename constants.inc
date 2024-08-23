; PPU registers.  These are the MMIO (memory-mapped I/O) addresses.
; MMIO is the method of performing input/output between the CPU and
; peripheral devices.  The PPU exposes 8 memory-mapped registers to
; the CPU, which normally sit at addresses $2000 - $2007.
PPUCTRL   = $2000  ; PPU control register
PPUMASK   = $2001  ; PPU mask register
PPUSTATUS = $2002  ; PPU status register
PPUADDR   = $2006  ; PPU address register
PPUDATA   = $2007

PPU_SPRRAM_ADDRESS = $2003 ; PPU SPR-RAM Address Register (Write)
PPU_SPRRAM_IO = $2004 ; PPU SPR-RAM I/O Register (Write)
PPU_VRAM_ADDRESS1 = $2005 ; PPU VRAM Address Register 1 (Write)
SPRITE_DMA = $4014 ; Sprite DMA Register

; Define APU Registers
APU_DM_CONTROL = $4010 ; APU Delta Modulation Control Register (Write)
APU_CLOCK = $4015 ; APU Sound/Vertical Clock Signal Register (Read/Write)

; Joystick/Controller values
JOYPAD1 = $4016 ; Joypad 1 (Read/Write)
JOYPAD2 = $4017 ; Joypad 2 (Read/Write)

; Gamepad bit values
PAD_A      = $01
PAD_B      = $02
PAD_SELECT = $04
PAD_START  = $08
PAD_U      = $10
PAD_D      = $20
PAD_L      = $40
PAD_R      = $80