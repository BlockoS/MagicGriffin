	.code
	.bank $000
	.org $0800
exit:
          rts     

read_sector:
          jsr     read_byte
l0804_00:
          cmp     #$d5
          bne     exit
          jsr     read_byte
          cmp     #$aa
          bne     l0804_00
          jsr     read_byte
          cmp     #$96
          beq     l081d_00
          cmp     #$ad
          bne     l0804_00
          jmp     l0992_00
l081d_00:
          jsr     initialize_crc
          ldx     #$00
l0822_00:
          jsr     read_byte
          sta     $0a07, X
          eor     $0a0c
          sta     $0a0c
          inx     
          cpx     #$05
          bcc     l0822_00
          jsr     read_byte_ex
          beq     l083b_00
          jmp     l09be_00
l083b_00:
          lda     $0a08
          sta     $0a04
          lda     $0a09
          sta     $0a05
          lda     $0a07
          cmp     #$00
          bne     l08aa_00
          jsr     initialize_crc
          ldy     #$00
          lda     $0a0b
          beq     l0883_00
          lda     $0a05
          sta     $086e
          lda     $0a04
          sta     $086d
l0864_00:
          bit     $c009
          bpl     l0864_00
          lda     $c008
          sta     $0000, Y
          eor     $0a0c
          sta     $0a0c
          iny     
          bne     l0864_00
          inc     $0a05
          inc     $086e
          dec     $0a0b
          bne     l0864_00
l0883_00:
          ldx     $0a0a
          beq     l08a4_00
          lda     #$8d
          sta     l0a03_00
l088d_00:
          bit     $c009
          bpl     l088d_00
          lda     $c008
          jsr     l0a03_00
          eor     $0a0c
          sta     $0a0c
          inc     $0a04
          dex     
          bne     l088d_00
l08a4_00:
          jsr     read_byte_ex
          jmp     l09c2_00
l08aa_00:
          cmp     #$01
          bne     l0902_00
          jsr     initialize_crc
          ldy     #$00
          lda     $0a0b
          beq     l08d6_00
          lda     #$b9
          sta     l0a03_00
l08bd_00:
          jsr     l0a03_00
          pha     
          jsr     l09e8_00
          pla     
          eor     $0a0c
          sta     $0a0c
          iny     
          bne     l08bd_00
          inc     $0a05
          dec     $0a0b
          bne     l08bd_00
l08d6_00:
          ldx     $0a0a
          beq     l08f4_00
          lda     #$ad
          sta     l0a03_00
l08e0_00:
          jsr     l0a03_00
          pha     
          jsr     l09e8_00
          pla     
          eor     $0a0c
          sta     $0a0c
          inc     $0a04
          dex     
          bne     l08e0_00
l08f4_00:
          lda     $0a0c
          jsr     l09e8_00
l08fa_00:
          bit     $c009
          bmi     l08fa_00
          jmp     lffc8_00

	.code
	.bank $000
	.org $09cd
initialize_crc:
          lda     #$81
          sta     $0a0c
          rts     

read_byte_ex:
          bit     $c009
          bpl     read_byte_ex
          lda     $c008
          eor     $0a0c
          rts     
read_byte:
          bit     $c009
          bpl     l09e3_00

l09e3_00:
          .db     $fb
          lda     $c008
          rts     

