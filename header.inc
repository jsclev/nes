.segment "HEADER"
INES_MAPPER = 0 ; 0 = NROM
INES_MIRROR = 0 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; The iNES ROM format always starts with a 4-byte magic number
                         ; of the characters "NES" followed by a $1A, the DOS end-of-file character
.byte $02 ; 16k PRG bank count
.byte $01 ; 8k CHR bank count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

; .segment "HEADER"
; .byte $4e, $45, $53, $1a ; Magic string that always begins an iNES header
; .byte $02        ; 2 16KB PRG-ROM banks
; .byte $01        ; 1 8KB CHR-ROM banks
; .byte %00000001  ; Vertical mirroring, no save RAM, no mapper
; .byte %00000000  ; No special-case flags set, no mapper
; .byte $00        ; No PRG-RAM present
; .byte $00        ; NTSC format