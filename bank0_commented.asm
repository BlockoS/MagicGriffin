;-------------------------------------------------------------------------------
; Disassembly of the Super Magic Griffin ROM.
; 
;-------------------------------------------------------------------------------
    .data
    .bank $000
    .org $e000
gfx_data:
    .incbin "gfx_data.bin"

	.code
	.bank $000
	.org $e400
; -------------------------------------------------------------------------------
;  Disable VDC interrupts.
; -------------------------------------------------------------------------------
video_disable_interrupts:               ; bank: $000 logical: $e400
          lda     #$05
          sta     video_reg
          lda     #$00
          sta     video_data_l
          rts     

; -------------------------------------------------------------------------------
;  Fill the BAT with 0..
; -------------------------------------------------------------------------------
video_clear_bat:                        ; bank: $000 logical: $e40b
          lda     #$00
          sta     video_reg
          sta     video_data_l
          sta     video_data_h
          lda     #$02
          sta     video_reg
          ldx     #$03
          ldy     #$00
          lda     #$00
@loop:                                  ; bank: $000 logical: $e421
          sta     video_data_l
          sta     video_data_h
          iny     
          bne     @loop
          dex     
          bpl     @loop
          rts     

; -------------------------------------------------------------------------------
;  Print a single character.
;  Parameters:
;     A: Character id.
;     Y: BAT address LSB
;     X: BAT address MSB
; -------------------------------------------------------------------------------
put_char:                               ; bank: $000 logical: $e42e
          pha     
          lda     #$00
          sta     video_reg
          sty     video_data_l
          stx     video_data_h
          lda     #$02
          sta     video_reg
          pla     
          sta     video_data_l
          lda     #$11
          sta     video_data_h
          rts     

; -------------------------------------------------------------------------------
;  Print message.
;  Parameters:
;     A: message id
; -------------------------------------------------------------------------------
put_message:                            ; bank: $000 logical: $e449
          asl     A
          tay     
          lda     message_pointer_table, Y
          sta     _si
          lda     message_pointer_table+1, Y
          sta     _si+1
          jmp     print_string_raw

; -------------------------------------------------------------------------------
;  Print a nul terminated string.
;  Parameters:
;     A: string id.
; -------------------------------------------------------------------------------
put_string:                             ; bank: $000 logical: $e45a
          asl     A
          tay     
          lda     pointer_table, Y
          sta     _si
          lda     pointer_table+1, Y
          sta     _si+1
print_string_raw:                       ; bank: $000 logical: $e468
          ldy     #$00
@set_cursor:                            ; bank: $000 logical: $e46a
          lda     #$00                  ; set VRAM write address.
          sta     video_reg
          lda     [_si], Y
          sta     video_data_l
          iny     
          lda     [_si], Y
          sta     video_data_h
          iny     
          lda     #$02                  ; set BAT.
          sta     video_reg
@put_char:                              ; bank: $000 logical: $e480
          lda     [_si], Y              ; stop if the character is '\0'.
          beq     @end
          iny     
          cmp     #$ff                  ; $ff means that the string will continue somewhere else.
          beq     @set_cursor           ; the next 2 bytes will be the BAT address where the next part will be printed.
          sta     video_data_l
          lda     #$11                  ; the font tiles are stored at $1000 in VRAM, and will  use palette #1.
          sta     video_data_h
          bne     @put_char
@end:                                   ; bank: $000 logical: $e493
          rts     

; -------------------------------------------------------------------------------
;  Read joypads.
;  Return:
;     joypad: Joypad states.
; -------------------------------------------------------------------------------
read_joypad:                            ; bank: $000 logical: $e494
          lda     #$03
          sta     joyport
          lda     #$01
          sta     joyport
          lda     joyport
          and     #$0f
          tax     
          lda     joypad_buttons, X
          sta     <joypad
          lda     #$00
          sta     joyport
          lda     joyport
          and     #$0f
          tax     
          lda     joypad_directions, X
          ora     <joypad
          sta     <joypad
          rts     

joypad_buttons:                         ; bank: $000 logical: $e4bc
          .db $0f,$07,$0e,$06,$0b,$03,$0a,$02
          .db $0d,$05,$0c,$04,$09,$01,$08,$00
joypad_directions:                      ; bank: $000 logical: $e4cc
          .db $f0,$70,$b0,$30,$d0,$50,$90,$10
          .db $e0,$60,$a0,$20,$c0,$40,$80,$00
; -------------------------------------------------------------------------------
;  Wait for joypad.
;  Return:
;     joypad: Joypad states.
; -------------------------------------------------------------------------------
wait_joypad:                            ; bank: $000 logical: $e4dc
          jsr     read_joypad
          lda     <joypad
          bne     wait_joypad
          rts     

vdc_init_table:                         ; bank: $000 logical: $e4e4
          .db $05,$80,$00
          .db $06,$00,$00
          .db $07,$00,$00
          .db $08,$00,$00
          .db $09,$80,$00
          .db $0a,$02,$02
          .db $0b,$20,$03
          .db $0c,$0f,$03
          .db $0d,$ef,$00
          .db $0e,$00,$00
          .db $0f,$00,$00
; -------------------------------------------------------------------------------
;  Initialize VDC, upload font to VRAM and setup font and background color.
; -------------------------------------------------------------------------------
video_init:                             ; bank: $000 logical: $e505
          lda     #$00
          sta     color_ctrl
          ldy     #$00
@l0:                                    ; bank: $000 logical: $e50c
          lda     vdc_init_table, Y
          sta     video_reg
          iny     
          lda     vdc_init_table, Y
          sta     video_data_l
          iny     
          lda     vdc_init_table, Y
          sta     video_data_h
          iny     
          cpy     #$21
          bcc     @l0
          jsr     video_clear_bat
          lda     #$00
          sta     _si
          lda     #$e0
          sta     _si+1
          lda     #$00
          sta     video_reg
          sta     video_data_l
          lda     #$10
          sta     video_data_h
          lda     #$02
          sta     video_reg
          jsr     video_load_1bpp_32
          inc     _si+1
          jsr     video_load_1bpp_32
          inc     _si+1
          jsr     video_load_1bpp_32
          inc     _si+1
          jsr     video_load_1bpp_32
          lda     #$00
          sta     color_reg_lo
          sta     color_reg_hi
          lda     #$06
          sta     color_data_lo
          lda     #$00
          sta     color_data_hi
          lda     #$11
          sta     color_reg_lo
          lda     #$ff
          sta     color_data_lo
          sta     color_data_hi
          rts     

; -------------------------------------------------------------------------------
;  Copy 1bpp tiles to VRAM.
;  Parameters:
;     _si: 1bpp tiles ROM address.
; -------------------------------------------------------------------------------
video_load_1bpp_32:                     ; bank: $000 logical: $e579
          ldy     #$00
@load_1bpp_tile:                        ; bank: $000 logical: $e57b
          ldx     #$08
@plane_01:                              ; bank: $000 logical: $e57d
          lda     [_si], Y
          sta     video_data_l
          lda     #$00
          sta     video_data_h
          iny     
          dex     
          bne     @plane_01
          ldx     #$08
          lda     #$00
@plane_23:                              ; bank: $000 logical: $e58f
          sta     video_data_l
          sta     video_data_h
          dex     
          bne     @plane_23
          cpy     #$00
          bne     @load_1bpp_tile
          rts     

; -------------------------------------------------------------------------------
;  Decompose a number to base 10.
;  Parameters:
;     A: Number to be converted.
;  Return:
;     $0018: unit.
;     $0019: ten.
;     $001a: hundred.
; -------------------------------------------------------------------------------
base10:                                 ; bank: $000 logical: $e59d
          ldx     #$00
          stx     $0018
          stx     $0019
          stx     $001a
@hundred:                               ; bank: $000 logical: $e5a8
          cmp     #$64
          bcc     @ten
          sbc     #$64
          inc     $001a
          jmp     @hundred
@ten:                                   ; bank: $000 logical: $e5b4
          cmp     #$0a
          bcc     @unit
          sbc     #$0a
          inc     $0019
          jmp     @ten
@unit:                                  ; bank: $000 logical: $e5c0
          sta     $0018
          rts     
; -------------------------------------------------------------------------------
;  Print a base 10 number and decrement it.
;  Parameters:
;     $0018: unit.
;     $0019: ten.
;     $001a: hundred.
; -------------------------------------------------------------------------------
print_dec_base10:                       ; bank: $000 logical: $e5c4
          lda     #$00                  ; set BAT address
          sta     video_reg
          lda     #$91
          sta     video_data_l
          lda     #$02
          sta     video_data_h
          lda     #$02
          sta     video_reg
          lda     $001a                 ; print hundred
          ora     #$30
          sta     video_data_l
          lda     #$11
          sta     video_data_h
          lda     $0019                 ; print ten
          ora     #$30
          sta     video_data_l
          lda     #$11
          sta     video_data_h
          lda     $0018                 ; print unit
          ora     #$30
          sta     video_data_l
          lda     #$11
          sta     video_data_h
          dec     $0018                 ; decrement number
          bpl     @end
          lda     #$09
          sta     $0018
          dec     $0019
          bpl     @end
          lda     #$09
          sta     $0019
          dec     $001a
@end:                                   ; bank: $000 logical: $e616
          rts     

bat_coords:                             ; bank: $000 logical: $e617
          .db $03,$08
          .db $03,$08
menu.item_count:                        ; bank: $000 logical: $e61b
          .db $06,$00,$02,$00
menu.callback_table_pointers:           ; bank: $000 logical: $e61f
          .dw main_menu.callbacks
          .dw unknown_menu.callbacks
main_menu.callbacks:                    ; bank: $000 logical: $e623
          .dw menu.run_file
          .dw menu.run_ic_card
          .dw menu.save_ic_card
          .dw menu.rename_file
          .dw menu.delete_file
          .dw menu.format_disk
unknown_menu.callbacks:                 ; bank: $000 logical: $e62f
          .dw $0f00
          .dw $0f03
pointer_table:                          ; bank: $000 logical: $e633
          .dw main_screen
          .dw utility_menu
          .dw reserved
          .dw logo
          .dw run_file
          .dw run_ic_card
          .dw save_ic_card
          .dw rename_file
          .dw delete_file
          .dw format_disk
          .dw loading
          .dw saving
          .dw renaming
          .dw deleting
          .dw formatting
          .dw one_mega_card
          .dw new_name
          .dw file_list
main_screen:                            ; bank: $000 logical: $e657
          .db $82,$00,"MAGIC GRIFFIN V-1",$ff
          .db $05,$01,"RUN FILE",$ff
          .db $45,$01,"RUN IC CARD",$ff
          .db $85,$01,"SAVE IC CARD",$ff
          .db $c5,$01,"RENAME FILE",$ff
          .db $05,$02,"DELETE FILE",$ff
          .db $45,$02,"FORMAT DISK",$ff
          .db $12,$02,"RAM: 8M",$ff 
          .db $12,$01,"[",$18,$19,"]-CHOOSE",$ff
          .db $42,$01,"[I]-ACCEPT",$00
utility_menu:                           ; bank: $000 logical: $e6e2
          .db $82,$00,"UTILITY MENU",$ff
reserved:                               ; bank: $000 logical: $e6f1
          .db $45,$01,"(RESERVED)",$00
logo:                                   ; bank: $000 logical: $e6fe
          .db $94,$00,$01,"1990 JSI",$ff
          .db $c7,$02,"`cf  ilo  rux  {~",$0d,$ff
          .db $e7,$02,"adg  jmp  svy  |",$7f,$0e,$ff
          .db $07,$03,"beh  knq  twz  }",$0c,$0f,$ff
          .db $48,$03,"FRONT FAREAST CO.",$00
run_file:                               ; bank: $000 logical: $e75d
          .db $82,$00,"RUN FILE",$ff
          .db $03,$01,"INSERT DISK...",$00
run_ic_card:                            ; bank: $000 logical: $e779
          .db $82,$00,"RUN IC CARD",$00
save_ic_card:                           ; bank: $000 logical: $e787
          .db $82,$00,"SAVE IC CARD",$00
rename_file:                            ; bank: $000 logical: $e796
          .db $82,$00,"RENAME FILE",$ff
          .db $03,$01,"INSERT DISK...",$00
delete_file:                            ; bank: $000 logical: $e7b5
          .db $82,$00,"DELETE FILE",$ff
          .db $03,$01,"INSERT DISK...",$00
format_disk:                            ; bank: $000 logical: $e7d4
          .db $82,$00,"FORMAT DISK",$ff
          .db $c5,$00,"FORMAT SELECT:",$ff
          .db $05,$01,"1.44M (HD)",$ff
          .db $45,$01,"720K  (2D)",$00
loading:                                ; bank: $000 logical: $e80d
          .db $85,$02,"LOADING....[000] ",$00
saving:                                 ; bank: $000 logical: $e821
          .db $85,$02,"SAVING.....[000] ",$00
renaming:                               ; bank: $000 logical: $e835
          .db $85,$02,"RENAMING...      ",$00
deleting:                               ; bank: $000 logical: $e849
          .db $85,$02,"DELETING...      ",$00
formatting:                             ; bank: $000 logical: $e85d
          .db $85,$02,"FORMATTING.[000] ",$00
one_mega_card:                          ; bank: $000 logical: $e871
          .db $c5,$00,"1M CARD",$00
new_name:                               ; bank: $000 logical: $e87b
          .db $03,$01,"  NEW NAME : ___________",$ff
          .db $45,$01,"0123456789ABCDEFGH",$ff
          .db $85,$01,"IJKLMNOPQRSTUVWXYZ",$ff
          .db $e5,$01,"[",$18,$19,$1b,$1a,"]-CHOOSE",$ff
          .db $05,$02,"[RUN]-CHAR INSERT",$ff
          .db $25,$02,"[SEL]-CHAR DELETE",$ff
          .db $45,$02,"[I]-ACCEPT  [II]-ABORT",$00
file_list:                              ; bank: $000 logical: $e911
          .db $c5,$00,"FILE LIST:",$ff
          .db $12,$01,"[SEL]-PAGE",$ff
          .db $52,$01,"[",$18,$19,"]-CHOOSE",$ff
          .db $92,$01,"[I]-ACCEPT",$ff
          .db $d2,$01,"[II]-ABORT",$ff
          .db $12,$02,"FILE:   ",$ff
          .db $53,$02,"FREE:  M",$00

message_pointer_table:                  ; bank: $000 logical: $e969
          .dw msg_ok
          .dw msg_err
          .dw msg_no_disk
          .dw msg_read_err
          .dw msg_write_err
          .dw msg_no_file
          .dw msg_write_protected
          .dw msg_file_not_found
          .dw msg_file_dup
          .dw msg_no_space
          .dw msg_no_card
          .dw msg_file_err

msg_ok:                                 ; bank: $000 logical: $e981
          .db $85,$02,"OK",$00
msg_err:                                ; bank: $000 logical: $e986
          .db $85,$02,"ERR             ",$00
msg_no_disk:                            ; bank: $000 logical: $e999
          .db $85,$02,"NO DISK         ",$00
msg_read_err:                           ; bank: $000 logical: $e9ac
          .db $85,$02,"READ ERR        ",$00
msg_write_err:                          ; bank: $000 logical: $e9bf
          .db $85,$02,"WRITE ERR       ",$00
msg_no_file:                            ; bank: $000 logical: $e9d2
          .db $85,$02,"NO FILE         ",$00
msg_write_protected:                    ; bank: $000 logical: $e9e5
          .db $85,$02,"WRITE PROTECTED ",$00
msg_file_not_found:                     ; bank: $000 logical: $e9f8
          .db $85,$02,"FILE NOT FOUND  ",$00
msg_file_dup:                           ; bank: $000 logical: $ea0b
          .db $85,$02,"DUP FILE NAME   ",$00
msg_no_space:                           ; bank: $000 logical: $ea1e
          .db $85,$02,"NOT ENOUGH SPACE",$00
msg_no_card:                            ; bank: $000 logical: $ea31
          .db $85,$02,"NO IC CARD      ",$00
msg_file_err:                           ; bank: $000 logical: $ea44
          .db $85,$02,"FILE ERR        ",$00
	.code
	.bank $000
	.org $eb00
; -------------------------------------------------------------------------------
;  Reset IRQ handler.
; -------------------------------------------------------------------------------
irq_reset:                              ; bank: $000 logical: $eb00
          sei                           ; disable interrupts
          cld                           ; clear decimal flag
          csl                           ; switch cpu to low speed mode
          lda     #$f8
          tam     #$00                  ; mpr 0 = RAM
          lda     #$f8
          tam     #$01                  ; mpr 1 = RAM
          lda     #$ff
          tam     #$02                  ; mpr 2 = I/O
          lda     #$01
          tam     #$06                  ; mpr 6 = 
          lda     #$02
          tam     #$05                  ; mpr 5 = 
          lda     #$03
          tam     #$04                  ; mpr 4 = 
                                        ; mpr 7 = 1st ROM bank (by default)
          ldx     #$ff
          stx     stack_pointer
          txs     
          jsr     unknown_f006
          ldy     #$00
@ramcode_init.0:                        ; bank: $000 logical: $eb26
          lda     ramcode+$000, Y
          sta     ramcode_dst, Y
          lda     ramcode+$100, Y
          sta     ramcode_dst+$100, Y
          lda     ramcode+$200, Y
          sta     ramcode_dst+$200, Y
          lda     ramcode+$300, Y
          sta     ramcode_dst+$300, Y
          lda     ramcode+$400, Y
          sta     ramcode_dst+$400, Y
          iny     
          bne     @ramcode_init.0

          ldy     #$00
          jmp     main

          sei     
          cld     
          csl     
          lda     #$f8
          tam     #$00
          lda     #$f8
          tam     #$01
          lda     #$ff
          tam     #$02
          lda     #$01
          tam     #$06
          lda     #$02
          tam     #$05
          lda     #$03
          tam     #$04
          ldx     #$ff
          txs     
          jsr     unknown_f006
          ldy     #$00
@ramcode_init.1:                        ; bank: $000 logical: $eb6f
          lda     ramcode+$000, Y
          sta     ramcode_dst, Y
          lda     ramcode+$100, Y
          sta     ramcode_dst+$100, Y
          lda     ramcode+$200, Y
          sta     ramcode_dst+$200, Y
          lda     ramcode+$300, Y
          sta     ramcode_dst+$300, Y
          lda     ramcode+$400, Y
          sta     ramcode_dst+$400, Y
          iny     
          bne     @ramcode_init.1
@loop:                                  ; bank: $000 logical: $eb90
          jsr     read_sector
          jmp     @loop

; -------------------------------------------------------------------------------
;  Main entry point.
; -------------------------------------------------------------------------------
main:                                   ; bank: $000 logical: $eb96
          sty     <$95
          jsr     video_init            ; setup VDC and VCE.
          lda     #$00
          sta     <$94
          ldy     <$95
          cpy     #$02
          bcc     loop
              ldy     #$00
              sty     <$95
loop:                                   ; bank: $000 logical: $eba9
          tya     
          asl     A
          tay     
          lda     menu.item_count, Y
          sta     <$93
          lda     menu.callback_table_pointers, Y
          sta     <$91
          lda     menu.callback_table_pointers+1, Y
          sta     <$92
          lda     bat_coords, Y
          tax     
          lda     bat_coords+1, Y
          tay     
          jsr     compute_cursor_bat_addr
          jsr     video_clear_bat
          lda     <$95
          jsr     put_string
          lda     #$03
          jsr     put_string
          jsr     unknown_face
@update:                                ; bank: $000 logical: $ebd6
          jsr     menu_update
          lda     <joypad
          bpl     @update
          jsr     video_clear_bat
          lda     #$03
          jsr     put_string
          lda     <$94
          clc     
          adc     #$04
          jsr     put_string
          lda     <$94
          asl     A
          tay     
          lda     [$91], Y
          sta     _si
          iny     
          lda     [$91], Y
          sta     _si+1
          lda     <$95
          pha     
          lda     <$94
          pha     
          jsr     run_submenu
          pla     
          sta     <$94
          pla     
          sta     <$95
          tay     
          jmp     loop

run_submenu:                            ; bank: $000 logical: $ec0f
          tsx     
          stx     <$90
          jmp     [_si]

menu_update:                            ; bank: $000 logical: $ec15
          ldx     #$10
          jsr     update_cursor
          jsr     wait_joypad
@loop:                                  ; bank: $000 logical: $ec1d
          bit     disk_status
          bpl     @no_disk
          jsr     read_sector
@no_disk:                               ; bank: $000 logical: $ec25
          jsr     read_joypad
          lda     <joypad
          beq     @loop
          bmi     @I
          and     #$0c
          bne     @dir
          ldx     #$20
          jsr     update_cursor
@I:                                     ; bank: $000 logical: $ec37
          rts     
@dir:                                   ; bank: $000 logical: $ec38
          ldx     #$20
          jsr     update_cursor
          lda     <joypad
          and     #$04
          bne     @down
@up:                                    ; bank: $000 logical: $ec43
          dec     <$94
          jmp     menu_update
@down:                                  ; bank: $000 logical: $ec48
          inc     <$94
          jmp     menu_update

update_cursor:                          ; bank: $000 logical: $ec4d
          lda     <$94
          bpl     lec56_00
          lda     <$93
          sec     
          sbc     #$01
lec56_00:                               ; bank: $000 logical: $ec56
          cmp     <$93
          bcc     lec5c_00
          lda     #$00
lec5c_00:                               ; bank: $000 logical: $ec5c
          sta     <$94
          tay     
          lda     #$00
          sta     video_reg
          lda     $0100, Y
          sta     video_data_l
          lda     $0120, Y
          sta     video_data_h
          lda     #$02
          sta     video_reg
          stx     video_data_l
          lda     #$11
          sta     video_data_h
          rts     

; -------------------------------------------------------------------------------
;  Compute the BAT address of the cursor.
;  Parameters:
;     X: X BAT coordinate.
;     Y: Y BAT coordinate.
;  Return:
;   $0100-$0109: MSB of the BAT addres of the menu lines.
;   $0120-$0129: LSB od the BAT address.
; -------------------------------------------------------------------------------
compute_cursor_bat_addr:                ; bank: $000 logical: $ec7e
          txa                           ; as the BAT is 32 by 32 the address is computed this way:
          asl     A                     ; addr = (X & 0x1f)+ Y*32
          asl     A
          asl     A
          sta     $0100
          tya     
          lsr     A
          ror     $0100
          lsr     A
          ror     $0100
          lsr     A
          ror     $0100                 ; $1000 now contains the LSB of the BAT address.
          sta     $0120                 ; $1200 its MSB.
          ldy     #$00
@next_row:                              ; bank: $000 logical: $ec97
          clc                           ; we compute the offset for the next 8 menu line.
          lda     $0100, Y
          adc     #$40                  ; jumps 2 lines.
          sta     $0101, Y
          lda     $0120, Y
          adc     #$00
          sta     $0121, Y
          iny     
          cpy     #$09
          bcc     @next_row
          rts     

    .data
    .bank $000
    .org  $ecae
ramcode:
    .incbin "ramcode.bin"

	.code
	.bank $000
	.org $f000
unknown_f000:                           ; bank: $000 logical: $f000
          lda     #$2d
          sta     $c002
          rts     

unknown_f006:                           ; bank: $000 logical: $f006
          lda     #$04
          sta     $c002
          rts     
lf00c_00:                               ; bank: $000 logical: $f00c
          bit     $c004                 ; c004 may be the main status register ($3f4 in the mcs3201 datasheet)
          bpl     lf00c_00              ; bit 7 = 1 -> ready for host to send or receive data
          bvc     lf00c_00              ; bit 6 = 0 -> data transfer from host to controller
          lda     $c005
          rts     
lf017_00:                               ; bank: $000 logical: $f017
          bit     $c004
          bpl     lf017_00              ; bit 7 = 1 -> ready for host to send or receive data
          bvs     lf017_00              ; bit 6 = 1 -> data transfer from controller to host
          sta     $c005
          rts     

	.code
	.bank $000
	.org $f6af
unknown_f6af:                           ; bank: $000 logical: $f6af
          ldx     <$24                  ; 2: format 1.44M, 3: format 720K
          lda     unknown_f797, X
          sta     $c007
          lda     $f7ab, X
          sta     <$27
          lda     $f79b, X
          sta     <$2f
          lda     unknown_f79f, X
          sta     <$25
          lda     $f7a3, X
          sta     <$26
          ldy     #$00
          tya     
lf6ce_00:                               ; bank: $000 logical: $f6ce
          sta     $0200, Y
          sta     $0300, Y
          dey     
          bne     lf6ce_00
          lda     $f7a7, X
          sta     $0200
          lda     #$ff
          sta     $0201
          sta     $0202
          lda     <$25
          jsr     base10
          jsr     lf0e7_00
          jsr     lf041_00
          jsr     lf5e5_00
          ldx     #$00
lf6f5_00:                               ; bank: $000 logical: $f6f5
          stx     <$32
          jsr     lf022_00
          jsr     print_dec_base10
          lda     #$01
          jsr     unknown_f751
          lda     #$05
          jsr     unknown_f751
          ldx     <$32
          bne     lf74b_00
          ldy     #$00
          sty     <$1c
          sty     <$1d
          ldy     #$02
          sty     <$1e
          jsr     lf1b3_00
          lda     #$eb
          sta     $0200
          lda     #$3e
          sta     $0201
          lda     #$90
          sta     $0202
          lda     #$01
          sta     $0234
          lda     <$24
          asl     A
          tax     
          lda     $f7af, X
          sta     <$0e
          lda     $f7b0, X
          sta     <$0f
          ldy     #$0e
lf73c_00:                               ; bank: $000 logical: $f73c
          lda     [$0e], Y
          sta     $020c, Y
          dey     
          bpl     lf73c_00
          dec     <$1e
          jsr     lf1b3_00
          ldx     <$32
lf74b_00:                               ; bank: $000 logical: $f74b
          inx     
          cpx     <$25
          bcc     lf6f5_00
          rts     

unknown_f751:                           ; bank: $000 logical: $f751
          tax     
          lsr     A
          lsr     A
          sta     <$33
          lda     #$4d
          jsr     lf017_00
          txa     
          jsr     lf017_00
          lda     <$35
          jsr     lf017_00
          lda     <$26
          jsr     lf017_00
          lda     <$27
          jsr     lf017_00
          lda     #$00
          jsr     lf017_00
          ldx     #$01
lf775_00:                               ; bank: $000 logical: $f775
          stx     <$34
          lda     <$32
          jsr     lf017_00
          lda     <$33
          jsr     lf017_00
          lda     <$34
          jsr     lf017_00
          lda     <$35
          jsr     lf017_00
          ldx     <$34
          inx     
          cpx     <$26
          beq     lf775_00
          bcc     lf775_00
          jmp     lf152_00

unknown_f797:                           ; bank: $000 logical: $f797
          .db $01,$00,$02,$00,$01,$00,$00,$00
unknown_f79f:                           ; bank: $000 logical: $f79f
          .db $28,$50,$50,$50,$09,$0f,$09,$12
          .db $fd,$f9,$f9,$f0,$50,$54,$50,$6c
          .db $b7,$f7,$c6,$f7,$d5,$f7,$e4,$f7
          .db $02,$02,$01,$00,$02,$70,$00,$d0
	.code
	.bank $000
	.org $f7f3
menu.run_file:                          ; bank: $000 logical: $f7f3
          jsr     lfb8a_00
          lda     #$0a
          jsr     put_string
          jsr     lf5fc_00
          jsr     lfa84_00
          jsr     unknown_f006
          jmp     l0a0d_248

menu.rename_file:                       ; bank: $000 logical: $f807
          jsr     lfb8a_00
          jsr     video_clear_bat
          lda     #$03
          jsr     put_string
          lda     #$07
          jsr     put_string
          jsr     lf931_00
          lda     #$0c
          jsr     put_string
          jsr     lf07d_00
          jsr     lf5e5_00
          jsr     lf449_00
          bcc     lf82f_00
          lda     #$08
          jmp     unknown_f913
lf82f_00:                               ; bank: $000 logical: $f82f
          jsr     lf43e_00
          bcs     lf839_00
          lda     #$07
          jmp     unknown_f913
lf839_00:                               ; bank: $000 logical: $f839
          ldx     #$0a
lf83b_00:                               ; bank: $000 logical: $f83b
          lda     <$40, X
          sta     <$4b, X
          dex     
          bpl     lf83b_00
          jsr     lf252_00
          jsr     unknown_f006
          rts     

menu.delete_file:                       ; bank: $000 logical: $f849
          jsr     lfb8a_00
          lda     #$0d
          jsr     put_string
          jsr     lf07d_00
          jsr     lf5e5_00
          jsr     lf43e_00
          bcs     lf861_00
          lda     #$07
          jmp     unknown_f913
lf861_00:                               ; bank: $000 logical: $f861
          jsr     lf47d_00
          lda     #$e5
          sta     <$4b
          jsr     lf252_00
          jsr     unknown_f006
          rts     

menu.format_disk:                       ; bank: $000 logical: $f86f
          ldx     #$03
          ldy     #$08
          jsr     compute_cursor_bat_addr
          lda     #$02
          sta     <$93
          lda     #$00
          sta     <$94
@loop:                                  ; bank: $000 logical: $f87e
          jsr     menu_update
          lda     <joypad
          and     #$c0
          beq     @loop
@check_button:                          ; bank: $000 logical: $f887
          and     #$80
          bne     @format_disk
@previous_menu:                         ; bank: $000 logical: $f88b
          rts     
@format_disk:                           ; bank: $000 logical: $f88c
          lda     #$0e
          jsr     put_string
          lda     <$94
          and     #$01
          eor     #$01
          ora     #$02
          sta     <$24
          jsr     unknown_f6af
          jsr     unknown_f006
          rts     

menu.run_ic_card:                       ; bank: $000 logical: $f8a2
          jsr     unknown_faeb
          jmp     l0a43_248

menu.save_ic_card:                      ; bank: $000 logical: $f8a8
          jsr     unknown_faeb
          jsr     lfb1b_00
          lda     #$0f
          jsr     put_string
          ldx     $0400
          lda     $f90b, X
          ora     #$30
          ldx     #$00
          ldy     #$c5
          jsr     put_char
          lda     #$56
          sta     $040e
          lda     #$31
          sta     $040f
          ldx     $0400
          lda     unknown_f8fb, X
          sta     $0400
          lda     unknown_f903, X
          sta     $0401
          lda     #$31
          sta     $0402
          lda     $f90b, X
          sta     <$15
          lda     #$01
          sta     <$14
          jsr     lf931_00
          lda     #$0b
          jsr     put_string
          jsr     lf618_00
          jsr     lfa2c_00
          jsr     unknown_f006
          rts     

unknown_f8fb:                           ; bank: $000 logical: $f8fb
          .db $10,$20,$30,$30,$40,$40,$60,$80
unknown_f903:                           ; bank: $000 logical: $f903
          .db $00,$00,$01,$01,$00,$00,$00,$00
          .db $01,$02,$03,$03,$04,$04,$06,$08
unknown_f913:                           ; bank: $000 logical: $f913
          pha     
          jsr     unknown_f006
          pla     
          bit     <$2a
          bpl     lf92d_00
          jsr     put_message
lf91f_00:                               ; bank: $000 logical: $f91f
          jsr     read_joypad
          lda     <$f5
          bne     lf91f_00
lf926_00:                               ; bank: $000 logical: $f926
          jsr     read_joypad
          lda     <$f5
          beq     lf926_00
lf92d_00:                               ; bank: $000 logical: $f92d
          ldx     <$90
          txs     
          rts     
lf931_00:                               ; bank: $000 logical: $f931
          lda     #$10
          jsr     put_string
          ldx     #$0a
          lda     #$20
lf93a_00:                               ; bank: $000 logical: $f93a
          sta     <$40, X
          dex     
          bpl     lf93a_00
          lda     #$00
          sta     <$94
          lda     #$ac
          sta     <$0e
          lda     #$01
          sta     <$0f
lf94b_00:                               ; bank: $000 logical: $f94b
          ldx     #$1e
          jsr     lf9f2_00
lf950_00:                               ; bank: $000 logical: $f950
          jsr     read_joypad
          lda     <$f5
          bne     lf950_00
lf957_00:                               ; bank: $000 logical: $f957
          jsr     read_joypad
          lda     <$f5
          beq     lf957_00
          bmi     lf98d_00
          asl     A
          bmi     lf98a_00
          asl     A
          bmi     lf984_00
          asl     A
          bmi     lf97e_00
          ldx     #$20
          jsr     lf9f2_00
          lda     <$f5
          lsr     A
          bcs     lf992_00
          lsr     A
          bcs     lf996_00
          lda     <$0e
          eor     #$c0
          sta     <$0e
          bne     lf94b_00
lf97e_00:                               ; bank: $000 logical: $f97e
          jsr     lf99a_00
          jmp     lf94b_00
lf984_00:                               ; bank: $000 logical: $f984
          jsr     lf9ba_00
          jmp     lf94b_00
lf98a_00:                               ; bank: $000 logical: $f98a
          jmp     lf92d_00
lf98d_00:                               ; bank: $000 logical: $f98d
          lda     <$94
          beq     lf94b_00
          rts     
lf992_00:                               ; bank: $000 logical: $f992
          inc     <$0e
          bne     lf94b_00
lf996_00:                               ; bank: $000 logical: $f996
          dec     <$0e
          bne     lf94b_00
lf99a_00:                               ; bank: $000 logical: $f99a
          lda     #$01
          sta     video_reg
          lda     <$0e
          and     #$df
          sta     video_data_l
          lda     #$01
          sta     video_data_h
          lda     video_data_l
          jsr     lf9c6_00
          lda     <$94
          cmp     #$0a
          beq     lf9b9_00
          inc     <$94
lf9b9_00:                               ; bank: $000 logical: $f9b9
          rts     
lf9ba_00:                               ; bank: $000 logical: $f9ba
          lda     #$20
          jsr     lf9c6_00
          dec     <$94
          bpl     lf9c5_00
          inc     <$94
lf9c5_00:                               ; bank: $000 logical: $f9c5
          rts     
lf9c6_00:                               ; bank: $000 logical: $f9c6
          ldx     <$94
          sta     <$40, X
          lda     #$00
          sta     video_reg
          lda     <$94
          clc     
          adc     #$10
          sta     video_data_l
          lda     #$01
          sta     video_data_h
          lda     #$02
          sta     video_reg
          lda     <$40, X
          cmp     #$20
          bne     lf9e9_00
          lda     #$5f
lf9e9_00:                               ; bank: $000 logical: $f9e9
          sta     video_data_l
          lda     #$11
          sta     video_data_h
          rts     
lf9f2_00:                               ; bank: $000 logical: $f9f2
          lda     <$0e
          cmp     #$64
          bne     lf9fa_00
          lda     #$76
lf9fa_00:                               ; bank: $000 logical: $f9fa
          cmp     #$77
          bne     lfa00_00
          lda     #$65
lfa00_00:                               ; bank: $000 logical: $fa00
          cmp     #$a4
          bne     lfa06_00
          lda     #$b6
lfa06_00:                               ; bank: $000 logical: $fa06
          cmp     #$b7
          bne     lfa0c_00
          lda     #$a5
lfa0c_00:                               ; bank: $000 logical: $fa0c
          sta     <$0e
          lda     #$00
          sta     video_reg
          lda     <$0e
          sta     video_data_l
          lda     <$0f
          sta     video_data_h
          lda     #$02
          sta     video_reg
          txa     
          sta     video_data_l
          lda     #$11
          sta     video_data_h
          rts     
lfa2c_00:                               ; bank: $000 logical: $fa2c
          lda     #$00
          sta     <$02
          lda     #$04
          sta     <$03
          jsr     lf3c1_00
          lda     $0400
          sta     <$29
          jsr     base10
          lda     #$00
          sta     <$28
lfa43_00:                               ; bank: $000 logical: $fa43
          jsr     lfa5b_00
          lda     $0401
          lsr     A
          bcc     lfa56_00
          lda     <$28
          cmp     #$20
          bne     lfa56_00
          lda     #$40
          sta     <$28
lfa56_00:                               ; bank: $000 logical: $fa56
          dec     <$29
          bne     lfa43_00
          rts     
lfa5b_00:                               ; bank: $000 logical: $fa5b
          jsr     print_dec_base10
          lda     <$28
          and     #$03
          tax     
          sta     gfx_data, X
          lda     <$28
          and     #$fc
          ora     #$02
          tam     #$05
          inc     <$28
          lda     #$00
          sta     <$02
          lda     #$a0
          sta     <$03
          lda     #$10
          sta     <$1b
lfa7c_00:                               ; bank: $000 logical: $fa7c
          jsr     lf3c1_00
          dec     <$1b
          bne     lfa7c_00
          rts     
lfa84_00:                               ; bank: $000 logical: $fa84
          lda     #$00
          sta     <$02
          lda     #$04
          sta     <$03
          jsr     lf3c1_00
          lda     $0400
          sta     <$29
          jsr     base10
          lda     #$00
          sta     <$28
lfa9b_00:                               ; bank: $000 logical: $fa9b
          jsr     lfaa3_00
          dec     <$29
          bne     lfa9b_00
          rts     
lfaa3_00:                               ; bank: $000 logical: $faa3
          jsr     print_dec_base10
          jsr     unknown_fabf
          inc     <$28
          lda     #$00
          sta     <$02
          lda     #$80
          sta     <$03
          lda     #$10
          sta     <$1b
lfab7_00:                               ; bank: $000 logical: $fab7
          jsr     lf3c1_00
          dec     <$1b
          bne     lfab7_00
          rts     

unknown_fabf:                           ; bank: $000 logical: $fabf
          lda     <$28
          and     #$03
          tax     
          sta     gfx_data, X
          lda     <$28
          ora     #$03
          tam     #$04
          rts     

unknown_face:                           ; bank: $000 logical: $face
          lda     #$7f
          sta     <$28
          jsr     unknown_fabf
          lda     #$38
          ldx     #$aa
          stx     $8000
          cpx     $8000
          beq     lfae3_00
          lda     #$34
lfae3_00:                               ; bank: $000 logical: $fae3
          ldx     #$02
          ldy     #$17
          jsr     put_char
          rts     

unknown_faeb:                           ; bank: $000 logical: $faeb
          lda     #$00
          sta     <$0e
          sta     $0400
          ldy     #$7f
lfaf4_00:                               ; bank: $000 logical: $faf4
          tya     
          and     #$fc
          ora     #$02
          tam     #$05
          tya     
          and     #$03
          tax     
          sta     gfx_data, X
          lda     $a0a0
          cmp     #$ff
          beq     lfb0b_00
          inc     <$0e
lfb0b_00:                               ; bank: $000 logical: $fb0b
          sta     $0200, Y
          dey     
          bpl     lfaf4_00
          lda     <$0e
          bne     lfb1a_00
          lda     #$0a
          jmp     unknown_f913
lfb1a_00:                               ; bank: $000 logical: $fb1a
          rts     
lfb1b_00:                               ; bank: $000 logical: $fb1b
          ldy     #$00
          lda     #$00
lfb1f_00:                               ; bank: $000 logical: $fb1f
          sta     $0400, Y
          sta     $0500, Y
          iny     
          bne     lfb1f_00
          lda     #$00
          sta     $0400
          ldy     #$1f
          lda     #$00
          ldx     #$40
          jsr     lfb73_00
          bcs     lfb40_00
          lda     #$02
          ora     $0400
          sta     $0400
lfb40_00:                               ; bank: $000 logical: $fb40
          ldy     #$1f
          lda     #$00
          ldx     #$20
          jsr     lfb73_00
          bcs     lfb5f_00
          lda     #$04
          ora     $0400
          sta     $0400
          ldy     #$1f
          lda     #$40
          ldx     #$60
          jsr     lfb73_00
          bcc     lfb6a_00
          rts     
lfb5f_00:                               ; bank: $000 logical: $fb5f
          ldy     #$0f
          lda     #$00
          ldx     #$10
          jsr     lfb73_00
          bcs     lfb72_00
lfb6a_00:                               ; bank: $000 logical: $fb6a
          lda     #$01
          ora     $0400
          sta     $0400
lfb72_00:                               ; bank: $000 logical: $fb72
          rts     
lfb73_00:                               ; bank: $000 logical: $fb73
          sta     <$0e
          stx     <$02
          lda     #$02
          sta     <$0f
          sta     <$03
lfb7d_00:                               ; bank: $000 logical: $fb7d
          lda     [$0e], Y
          cmp     [$02], Y
          beq     lfb85_00
          clc     
          rts     
lfb85_00:                               ; bank: $000 logical: $fb85
          dey     
          bpl     lfb7d_00
          sec     
          rts     
lfb8a_00:                               ; bank: $000 logical: $fb8a
          ldy     #$00
          sty     <$95
          lda     #$20
lfb90_00:                               ; bank: $000 logical: $fb90
          sta     $0400, Y
          sta     $0500, Y
          sta     $0600, Y
          iny     
          bne     lfb90_00
          lda     #$23
          sta     <$96
          lda     #$05
          sta     <$94
          lda     #$00
          sta     <$0e
          lda     #$04
          sta     <$0f
lfbac_00:                               ; bank: $000 logical: $fbac
          ldy     #$00
          ldx     <$94
          lda     $fca7, X
          sta     [$0e], Y
          iny     
          lda     $fcad, X
          sta     [$0e], Y
          iny     
          lda     #$ff
          dec     <$94
          bpl     lfbc8_00
          ldy     #$05
          sty     <$94
          lda     #$00
lfbc8_00:                               ; bank: $000 logical: $fbc8
          ldy     #$10
          sta     [$0e], Y
          clc     
          lda     <$0e
          adc     #$11
          sta     <$0e
          lda     <$0f
          adc     #$00
          sta     <$0f
          dec     <$96
          bpl     lfbac_00
          ldx     #$02
          ldy     #$08
          jsr     compute_cursor_bat_addr
          lda     #$00
          sta     <$20
          sta     <$96
          jsr     lf07d_00
lfbed_00:                               ; bank: $000 logical: $fbed
          jsr     lfcb3_00
          bcc     lfc24_00
          lda     <$96
          ldy     #$11
          jsr     lfcd0_00
          ldy     #$02
          ldx     <$96
          lda     $f59c, X
          sta     [$0e], Y
          iny     
          lda     $f5c1, X
          sta     [$0e], Y
          iny     
          lda     #$2d
          sta     [$0e], Y
          lda     #$05
          jsr     lfce9_00
          ldy     #$0a
lfc14_00:                               ; bank: $000 logical: $fc14
          lda     $004b, Y
          sta     [$0e], Y
          dey     
          bpl     lfc14_00
          inc     <$96
          lda     <$96
          cmp     #$24
          bcc     lfbed_00
lfc24_00:                               ; bank: $000 logical: $fc24
          lda     #$11
          jsr     put_string
          jsr     lf546_00
          jsr     unknown_f006
          lda     <$96
          bne     lfc38_00
          lda     #$05
          jmp     unknown_f913
lfc38_00:                               ; bank: $000 logical: $fc38
          lda     <$95
          ldy     #$66
          jsr     lfcd0_00
          jsr     print_string_raw
          lda     <$95
          ldy     #$06
          jsr     lfcd0_00
          sec     
          lda     <$96
          sbc     <$0e
          cmp     #$06
          bcc     lfc56_00
          beq     lfc56_00
          lda     #$06
lfc56_00:                               ; bank: $000 logical: $fc56
          sta     <$93
          lda     #$00
          sta     <$94
lfc5c_00:                               ; bank: $000 logical: $fc5c
          jsr     menu_update
          bit     <$f5
          bvs     lfc81_00
          bmi     lfc84_00
          lda     <$f5
          and     #$20
          beq     lfc5c_00
          inc     <$95
          lda     <$95
          ldy     #$06
          jsr     lfcd0_00
          lda     <$0e
          cmp     <$96
          bcc     lfc38_00
          lda     #$00
          sta     <$95
          jmp     lfc38_00
lfc81_00:                               ; bank: $000 logical: $fc81
          jmp     lf92d_00
lfc84_00:                               ; bank: $000 logical: $fc84
          lda     <$95
          ldy     #$06
          jsr     lfcd0_00
          lda     <$94
          jsr     lfce9_00
          lda     <$0e
          ldy     #$11
          jsr     lfcd0_00
          lda     #$05
          jsr     lfce9_00
          ldy     #$0a
lfc9e_00:                               ; bank: $000 logical: $fc9e
          lda     [$0e], Y
          sta     $0080, Y
          dey     
          bpl     lfc9e_00
          rts     
          tma     #$00
          tdd     $4383, $0203, $0102
          ora     [$01, X]
          ora     [$20, X]
          bsr     lfca8_00
          inc     <$20
          lda     <$4b
          beq     lfcce_00
          cmp     #$e5
          beq     lfcc8_00
          lda     <$56
          and     #$1c
          bne     lfcc8_00
          sec     
          rts     
lfcc8_00:                               ; bank: $000 logical: $fcc8
          ldx     <$20
          cpx     <$2d
          bcc     lfcb3_00
lfcce_00:                               ; bank: $000 logical: $fcce
          clc     
          rts     
lfcd0_00:                               ; bank: $000 logical: $fcd0
          ldx     #$00
          stx     <$0e
          ldx     #$04
          stx     <$0f
          tax     
lfcd9_00:                               ; bank: $000 logical: $fcd9
          clc     
          txa     
          adc     <$0e
          sta     <$0e
          lda     <$0f
          adc     #$00
          sta     <$0f
          dey     
          bne     lfcd9_00
          rts     
lfce9_00:                               ; bank: $000 logical: $fce9
          clc     
          adc     <$0e
          sta     <$0e
          lda     <$0f
          adc     #$00
          sta     <$0f
          rts     
          sei     
          csl     
          cld     
          tma     #$00
          pha     
          tma     #$01
          pha     
          tma     #$02
          pha     
          tma     #$03
          pha     
          tma     #$04
          pha     
          tma     #$05
          pha     
          tma     #$06
          pha     
          tma     #$07
          pha     
          lda     #$f8
          tam     #$00
          tam     #$01
          lda     #$ff
          tam     #$02
          lda     #$c0
          tam     #$03
          lda     #$03
          tam     #$04
          lda     #$02
          tam     #$05
          lda     #$01
          tam     #$06
          lda     #$00
          tam     #$07
          sta     $e004
          jsr     lfd51_00
          sta     $e006
          tax     
          pla     
          tam     #$07
          pla     
          tam     #$06
          pla     
          tam     #$05
          pla     
          tam     #$04
          pla     
          tam     #$03
          pla     
          tam     #$02
          pla     
          tam     #$01
          pla     
          tam     #$00
          rts     
lfd51_00:                               ; bank: $000 logical: $fd51
          lda     #$00
          sta     <$2a
          tsx     
          stx     <$90
          lda     <$8f
          beq     lfd63_00
          cmp     #$01
          beq     lfd7a_00
          ldx     #$01
          rts     
lfd63_00:                               ; bank: $000 logical: $fd63
          jsr     lf5fc_00
          jsr     lfdc2_00
          lda     <$8d
          sta     <$28
lfd6d_00:                               ; bank: $000 logical: $fd6d
          jsr     lfaa6_00
          dec     <$8e
          bne     lfd6d_00
          jsr     unknown_f006
          lda     #$00
          rts     
lfd7a_00:                               ; bank: $000 logical: $fd7a
          ldy     #$0a
lfd7c_00:                               ; bank: $000 logical: $fd7c
          lda     $0080, Y
          sta     $0040, Y
          dey     
          bpl     lfd7c_00
          lda     #$00
          sta     <$15
          lda     <$8e
          asl     A
          rol     <$15
          asl     A
          rol     <$15
          asl     A
          rol     <$15
          asl     A
          rol     <$15
          sta     <$14
          jsr     lf07d_00
          jsr     lf5e5_00
          jsr     lf43e_00
          bcc     lfdae_00
          jsr     lf47d_00
          lda     #$e5
          sta     <$4b
          jsr     lf252_00
lfdae_00:                               ; bank: $000 logical: $fdae
          jsr     lf618_00
          lda     <$8d
          sta     <$28
lfdb5_00:                               ; bank: $000 logical: $fdb5
          jsr     lfaa6_00
          dec     <$8e
          bne     lfdb5_00
          jsr     unknown_f006
          lda     #$00
          rts     
lfdc2_00:                               ; bank: $000 logical: $fdc2
          lda     <$8b
          ora     <$8c
          bne     lfdc9_00
          rts     
lfdc9_00:                               ; bank: $000 logical: $fdc9
          lda     <$2f
          bpl     lfde6_00
          asl     A
          bpl     lfddb_00
          asl     A
          bpl     lfddb_00
          inc     <$0c
          bne     lfde9_00
          inc     <$0d
          bne     lfde9_00
lfddb_00:                               ; bank: $000 logical: $fddb
          jsr     lf262_00
          lda     <$00
          sta     <$08
          lda     <$01
          sta     <$09
lfde6_00:                               ; bank: $000 logical: $fde6
          jsr     lf362_00
lfde9_00:                               ; bank: $000 logical: $fde9
          jsr     lf394_00
          lda     <$2f
          eor     #$20
          ora     #$80
          sta     <$2f
          dec     <$8b
          lda     <$8b
          cmp     #$ff
          bne     lfdc2_00
          dec     <$8c
          jmp     lfdc2_00

	.data
	.bank $000
	.org $ff0c
copyright:                              ; bank: $000 logical: $ff0c
          .db "*** MAGIC GRIFFIN V-1 ***"
          .db "COPYRIGHT 1990 BY JSI, FRONT FAREAST CO."
          .db "ALL RIGHTS RESERVED 11/12/90"
	.data
	.bank $000
	.org $fff6
irq_table:                              ; bank: $000 logical: $fff6
          .dw $ffff
          .dw $ffff
          .dw $ffff
          .dw $ffff
          .dw irq_reset
