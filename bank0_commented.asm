    .data
    .bank $000
    .org $e000
gfx_data:
    .incbin "gfx_data.bin"

	.code
	.bank $000
	.org $e400
video_disable_interrupts:
          lda     #$05
          sta     video_reg
          lda     #$00
          sta     video_data_l
          rts     

video_clear_bat:
          lda     #$00
          sta     video_reg
          sta     video_data_l
          sta     video_data_h
          lda     #$02
          sta     video_reg
          ldx     #$03
          ldy     #$00
          lda     #$00
@loop:
          sta     video_data_l
          sta     video_data_h
          iny     
          bne     @loop
          dex     
          bpl     @loop
          rts     

put_char:
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

put_message:
          asl     A
          tay     
          lda     message_pointer_table, Y
          sta     _si
          lda     $e96a, Y
          sta     _si+1
          jmp     le468_00

; Print a nul terminated string.
; Parameters:
;    A: string id.
;
put_string:
          asl     A
          tay     
          lda     pointer_table, Y
          sta     _si
          lda     pointer_table+1, Y
          sta     _si+1
le468_00:
          ldy     #$00
@set_cursor:
          lda     #$00                          ; Set VRAM write address.
          sta     video_reg
          lda     [_si], Y
          sta     video_data_l
          iny     
          lda     [_si], Y
          sta     video_data_h
          iny     
          lda     #$02                          ; Set BAT.
          sta     video_reg
@put_char:
          lda     [_si], Y                      ; Stop if the character is '\0'. 
          beq     @end
          iny     
          cmp     #$ff                          ; $ff means that the string will continue somewhere else.
          beq     @set_cursor                   ; The next 2 bytes will be the BAT address where the next part will be printed.
          sta     video_data_l
          lda     #$11                          ; The font tiles are stored at $1000 in VRAM, and will  use palette #1.  
          sta     video_data_h
          bne     @put_char
@end:
          rts     

read_joypad:
          lda     #$03
          sta     joyport
          lda     #$01
          sta     joyport
          lda     joyport
          and     #$0f
          tax     
          lda     joypad_buttons, X
          sta     <$f5
          lda     #$00
          sta     joyport
          lda     joyport
          and     #$0f
          tax     
          lda     joypad_directions, X
          ora     <$f5
          sta     <$f5
          rts     

joypad_buttons:
          .db $0f,$07,$0e,$06,$0b,$03,$0a,$02
          .db $0d,$05,$0c,$04,$09,$01,$08,$00
joypad_directions:
          .db $f0,$70,$b0,$30,$d0,$50,$90,$10
          .db $e0,$60,$a0,$20,$c0,$40,$80,$00
wait_joypad:
          jsr     read_joypad
          lda     <$f5
          bne     wait_joypad
          rts     

vdc_init_table:
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
video_init:
          lda     #$00
          sta     color_ctrl
          ldy     #$00
@l0:
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

video_load_1bpp_32:
          ldy     #$00
@load_1bpp_tile:
          ldx     #$08
@plane_01:
          lda     [$0e], Y
          sta     video_data_l
          lda     #$00
          sta     video_data_h
          iny     
          dex     
          bne     @plane_01
          ldx     #$08
          lda     #$00
@plane_23:
          sta     video_data_l
          sta     video_data_h
          dex     
          bne     @plane_23
          cpy     #$00
          bne     @load_1bpp_tile
          rts     

unknown_e59d:
          ldx     #$00
          stx     $0018
          stx     $0019
          stx     $001a
le5a8_00:
          cmp     #$64
          bcc     le5b4_00
          sbc     #$64
          inc     $001a
          jmp     le5a8_00
le5b4_00:
          cmp     #$0a
          bcc     le5c0_00
          sbc     #$0a
          inc     $0019
          jmp     le5b4_00
le5c0_00:
          sta     $0018
          rts     
          lda     #$00
          sta     video_reg
          lda     #$91
          sta     video_data_l
          lda     #$02
          sta     video_data_h
          lda     #$02
          sta     video_reg
          lda     $001a
          ora     #$30
          sta     video_data_l
          lda     #$11
          sta     video_data_h
          lda     $0019
          ora     #$30
          sta     video_data_l
          lda     #$11
          sta     video_data_h
          lda     $0018
          ora     #$30
          sta     video_data_l
          lda     #$11
          sta     video_data_h
          dec     $0018
          bpl     le616_00
          lda     #$09
          sta     $0018
          dec     $0019
          bpl     le616_00
          lda     #$09
          sta     $0019
          dec     $001a
le616_00:
          rts     

bat_coords:
          .db $03,$08
          .db $03,$08
menu.item_count:
          .db $06,$00,$02,$00
menu.callback_table_pointers:
          .dw $e623
          .dw $e62f
main_menu.callbacks:
          .dw $f7f3
          .dw $f8a2
          .dw $f8a8
          .dw $f807
          .dw $f849
          .dw $f86f
unknown_menu.callbacks:
          .dw $0f00
          .dw $0f03
pointer_table:
          .dw $e657
          .dw $e6e2
          .dw $e6f1
          .dw $e6fe
          .dw $e75d
          .dw $e779
          .dw $e787
          .dw $e796
          .dw $e7b5
          .dw $e7d4
          .dw $e80d
          .dw $e821
          .dw $e835
          .dw $e849
          .dw $e85d
          .dw $e871
          .dw $e87b
          .dw $e911
strings:
          db $82,$00,"MAGIC GRIFFIN V-1",$ff
          db $05,$01,"RUN FILE",$ff
          db $45,$01,"RUN IC CARD",$ff
          db $85,$01,"SAVE IC CARD",$ff
          db $c5,$01,"RENAME FILE",$ff
          db $05,$02,"DELETE FILE",$ff
          db $45,$02,"FORMAT DISK",$ff
          db $12,$02,"RAM: 8M",$ff 
          db $12,$01,"[",$18,$19,"]-CHOOSE",$ff
          db $52,$01,"[I]-ACCEPT",$00,$82,$00,"UTILITY MENU",$ff
          db $45,$01,"(RESERVED)",$00,$94,$00,$01,"1990 JSI",$ff
          db $c7,$02,"`cf  ilo  rux  {~",$0d,$ff
          db $e7,$02,"adg  jmp  svy  |",$7f,$0e,$ff
          db $07,$03,"beh  knq  twz  }",$0c,$0f,$ff
          db $48,$03,"FRONT FAREAST CO.",$00
          db $82,$00,"RUN FILE",$ff
          db $03,$01,"INSERT DISK...",$00
          dn $82,$00,"RUN IC CARD",$00
          db $82,$00,"SAVE IC CARD",$00
          db $82,$00,"RENAME FILE",$ff
          db $03,$01,"INSERT DISK...",$00
          db $82,$00,"DELETE FILE",$ff
          db $03,$01,"INSERT DISK...",$00
          db $82,$00,"FORMAT DISK",$ff
          db $c5,$00,"FORMAT SELECT:",$ff
          db $05,$01,"1.44M (HD)",$ff
          db $45,$01,"720K  (2D)",$00
          db $85,$02,"LOADING....[000] "$00
          db $85,$02,"SAVING.....[000] ",$00
          db $85,$02,"RENAMING...      ",$00
          db $85,$02,"DELETING...      ",$00
          db $85,$02,"FORMATTING.[000] ",$00
          db $c5,$00,"1M CARD",$00
          db $03,$01,"  NEW NAME : ___________",$ff
          db $45,$01,"0123456789ABCDEFGH",$ff
          db $85,$01,"IJKLMNOPQRSTUVWXYZ",$ff
          db $e5,$01,"[",$18,$19,$1b,$1a,"]-CHOOSE",$ff
          db $05,$02,"[RUN]-CHAR INSERT",$ff
          db $25,$02,"[SEL]-CHAR DELETE",$ff
          db $45,$02,"[I]-ACCEPT  [II]-ABORT",$00
          db $c5,$00,"FILE LIST:",$ff
          db $12,$01,"[SEL]-PAGE",$ff
          db $52,$01,"[",$18,$19,"]-CHOOSE",$ff
          db $92,$01,"[I]-ACCEPT",$ff
          db $d2,$01,"[II]-ABORT",$ff
          db $12,$02,"FILE:   ",$ff
          db $53,$02,"FREE:  M",$00

message_pointer_table:
          .dw $e981
          .dw $e986
          .dw $e999
          .dw $e9ac
          .dw $e9bf
          .dw $e9d2
          .dw $e9e5
          .dw $e9f8
          .dw $ea0b
          .dw $ea1e
          .dw $ea31
          .dw $ea44

messages:
          db $85,$02,"OK",$00
          db $85,$02,"ERR             ",$00
          db $85,$02,"NO DISK         ",$00
          db $85,$02,"READ ERR        ",$00
          db $85,$02,"WRITE ERR       ",$00
          dn $85,$02,"NO FILE         ",$00
          db $85,$02,"WRITE PROTECTED ",$00
          db $85,$02,"FILE NOT FOUND  ",$00
          db $85,$02,"DUP FILE NAME   ",$00
          db $85,$02,"NOT ENOUGH SPACE",$00 
          db $85,$02,"NO IC CARD      ",$00
          db $85,$02,"FILE ERR        ",$00
          
	.code
	.bank $000
	.org $eb00
irq_reset:
          sei                                       ; disable interrupts
          cld                                       ; clear decimal flag
          csl                                       ; switch cpu to low speed mode
          lda     #$f8
          tam     #$00                              ; mpr 0 = RAM 
          lda     #$f8
          tam     #$01                              ; mpr 1 = RAM
          lda     #$ff
          tam     #$02                              ; mpr 2 = 1st ROM bank
          lda     #$01
          tam     #$06                              ; mpr 6 = 1
          lda     #$02
          tam     #$05                              ; mpr 5 = 2
          lda     #$03
          tam     #$04                              ; mpr 4 = 3
          ldx     #$ff
          stx     stack_pointer
          txs                                       ; reset stack pointer
          jsr     unknown_f006

          ldy     #$00
@ramcode_init.0:
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
@ramcode_init.1:
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
@loop:
          jsr     l0801_248
          jmp     @loop

main:
          sty     <$95
          jsr     video_init

          lda     #$00
          sta     <$94
          ldy     <$95
          cpy     #$02
          bcc     loop
              ldy     #$00
              sty     <$95
loop:
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
@update:
          jsr     menu_update
          lda     <$f5
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

run_submenu:
          tsx     
          stx     <$90
          jmp     [_si]

menu_update:
          ldx     #$10
          jsr     update_cursor
          jsr     wait_joypad
@loop:
          bit     $c009
          bpl     @no_disk
          jsr     l0801_248
@no_disk:
          jsr     read_joypad
          lda     <$f5
          beq     @loop
          bmi     @I
          and     #$0c
          bne     @dir
          ldx     #$20
          jsr     update_cursor
@I:
          rts     
@dir:
          ldx     #$20
          jsr     update_cursor
          lda     <$f5
          and     #$04
          bne     @down
@up:
          dec     <$94
          jmp     menu_update
@down:
          inc     <$94
          jmp     menu_update

update_cursor:
          lda     <$94
          bpl     lec56_00
          lda     <$93
          sec     
          sbc     #$01
lec56_00:
          cmp     <$93
          bcc     lec5c_00
          lda     #$00
lec5c_00:
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

; Compute the BAT address of the cursor.
; Parameters:
;   X: X BAT coordinate.
;   Y: Y BAT coordinate.
; Return:
;  $0100-$0109: MSB of the BAT addres of the menu lines.
;  $0120-$0129: LSB od the BAT address.
;
compute_cursor_bat_addr:
          txa                                   ; As the BAT is 32 by 32 the address is computed this way:
          asl     A                             ; addr = (X & 0x1f)+ Y*32
          asl     A
          asl     A
          sta     $0100
          tya     
          lsr     A
          ror     $0100
          lsr     A
          ror     $0100
          lsr     A
          ror     $0100                         ; $1000 now contains the LSB of the BAT address.
          sta     $0120                         ; $1200 its MSB.
          ldy     #$00
@next_row:                                      ; We compute the offset for the next 8 menu line.
          clc     
          lda     $0100, Y
          adc     #$40                          ; Jumps 2 lines.
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
unknown_f000:
          lda     #$2d
          sta     $c002
          rts     

unknown_f006:
          lda     #$04
          sta     $c002
          rts     

	.code
	.bank $000
	.org $f6af
unknown_f6af:
          ldx     <$24
          lda     $f797, X
          sta     $c007
          lda     $f7ab, X
          sta     <$27
          lda     $f79b, X
          sta     <$2f
          lda     $f79f, X
          sta     <$25
          lda     $f7a3, X
          sta     <$26
          ldy     #$00
          tya     
lf6ce_00:
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
          jsr     unknown_e59d
          jsr     lf0e7_00
          jsr     lf041_00
          jsr     lf5e5_00
          ldx     #$00
lf6f5_00:
          stx     <$32
          jsr     lf022_00
          jsr     le5c4_00
          lda     #$01
          jsr     lf751_00
          lda     #$05
          jsr     lf751_00
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
lf73c_00:
          lda     [$0e], Y
          sta     $020c, Y
          dey     
          bpl     lf73c_00
          dec     <$1e
          jsr     lf1b3_00
          ldx     <$32
lf74b_00:
          inx     
          cpx     <$25
          bcc     lf6f5_00
          rts     

	.code
	.bank $000
	.org $f7f3
menu.run_file:
          jsr     lfb8a_00
          lda     #$0a
          jsr     put_string
          jsr     lf5fc_00
          jsr     lfa84_00
          jsr     unknown_f006
          jmp     l0a0d_248

menu.rename_file:
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
          jmp     lf913_00
lf82f_00:
          jsr     lf43e_00
          bcs     lf839_00
          lda     #$07
          jmp     lf913_00
lf839_00:
          ldx     #$0a
lf83b_00:
          lda     <$40, X
          sta     <$4b, X
          dex     
          bpl     lf83b_00
          jsr     lf252_00
          jsr     unknown_f006
          rts     

menu.delete_file:
          jsr     lfb8a_00
          lda     #$0d
          jsr     put_string
          jsr     lf07d_00
          jsr     lf5e5_00
          jsr     lf43e_00
          bcs     lf861_00
          lda     #$07
          jmp     lf913_00
lf861_00:
          jsr     lf47d_00
          lda     #$e5
          sta     <$4b
          jsr     lf252_00
          jsr     unknown_f006
          rts     

menu.format_disk:
          ldx     #$03
          ldy     #$08
          jsr     compute_cursor_bat_addr
          lda     #$02
          sta     <$93
          lda     #$00
          sta     <$94
@loop:
          jsr     menu_update
          lda     <$f5
          and     #$c0
          beq     @loop
@check_button:
          and     #$80
          bne     @format_disk
@previous_menu:
          rts     
@format_disk:
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

menu.run_ic_card:
          jsr     lfaeb_00
          jmp     l0a43_248

menu.save_ic_card:
          jsr     lfaeb_00
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
          lda     $f8fb, X
          sta     $0400
          lda     $f903, X
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

	.code
	.bank $000
	.org $fabf
unknown_fabf:
          lda     <$28
          and     #$03
          tax     
          sta     gfx_data, X
          lda     <$28
          ora     #$03
          tam     #$04
          rts     

unknown_face:
          lda     #$7f
          sta     <$28
          jsr     unknown_fabf
          lda     #$38
          ldx     #$aa
          stx     $8000
          cpx     $8000
          beq     lfae3_00
          lda     #$34
lfae3_00:
          ldx     #$02
          ldy     #$17
          jsr     put_char
          rts     

	.data
	.bank $000
	.org $ff0c
copyright:
          db "*** MAGIC GRIFFIN V-1 ***"
          db "COPYRIGHT 1990 BY JSI, FRONT FAREAST CO."
          db "ALL RIGHTS RESERVED 11/12/9"

	.data
	.bank $000
	.org $fff6
irq_table:
          .dw $ffff
          .dw $ffff
          .dw $ffff
          .dw $ffff
          .dw $eb00
