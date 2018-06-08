;    Filename: display.asm                                                     *
;    Date:   7/6                                                               *
;    Description: LCD display routines                                         *
;                                                                              *
    
;*******************************************************************************
; PIC18F46K22 Configuration Bit Settings

; Assembly source line config statements
; Processor Inclusion
    
#include "p18f46k22.inc"

; Configuration Word Setup

; CONFIG1H
  CONFIG  FOSC = INTIO7         ; Oscillator Selection bits (Internal oscillator block, CLKOUT function on OSC2)
  CONFIG  PLLCFG = OFF          ; 4X PLL Enable (Oscillator used directly)
  CONFIG  PRICLKEN = ON         ; Primary clock enable bit (Primary clock is always enabled)
  CONFIG  FCMEN = OFF           ; Fail-Safe Clock Monitor Enable bit (Fail-Safe Clock Monitor disabled)
  CONFIG  IESO = OFF            ; Internal/External Oscillator Switchover bit (Oscillator Switchover mode disabled)

; CONFIG2L
  CONFIG  PWRTEN = OFF          ; Power-up Timer Enable bit (Power up timer disabled)
  CONFIG  BOREN = SBORDIS       ; Brown-out Reset Enable bits (Brown-out Reset enabled in hardware only (SBOREN is disabled))
  CONFIG  BORV = 190            ; Brown Out Reset Voltage bits (VBOR set to 1.90 V nominal)

; CONFIG2H
  CONFIG  WDTEN = OFF           ; Watchdog Timer Enable bits (Watch dog timer is always disabled. SWDTEN has no effect.)
  CONFIG  WDTPS = 32768         ; Watchdog Timer Postscale Select bits (1:32768)

; CONFIG3H
  CONFIG  CCP2MX = PORTC1       ; CCP2 MUX bit (CCP2 input/output is multiplexed with RC1)
  CONFIG  PBADEN = ON           ; PORTB A/D Enable bit (PORTB<5:0> pins are configured as analog input channels on Reset)
  CONFIG  CCP3MX = PORTB5       ; P3A/CCP3 Mux bit (P3A/CCP3 input/output is multiplexed with RB5)
  CONFIG  HFOFST = ON           ; HFINTOSC Fast Start-up (HFINTOSC output and ready status are not delayed by the oscillator stable status)
  CONFIG  T3CMX = PORTC0        ; Timer3 Clock input mux bit (T3CKI is on RC0)
  CONFIG  P2BMX = PORTD2        ; ECCP2 B output mux bit (P2B is on RD2)
  CONFIG  MCLRE = EXTMCLR       ; MCLR Pin Enable bit (MCLR pin enabled, RE3 input pin disabled)

; CONFIG4L
  CONFIG  STVREN = ON           ; Stack Full/Underflow Reset Enable bit (Stack full/underflow will cause Reset)
  CONFIG  LVP = OFF             ; Single-Supply ICSP Enable bit (Single-Supply ICSP disabled)
  CONFIG  XINST = OFF           ; Extended Instruction Set Enable bit (Instruction set extension and Indexed Addressing mode disabled (Legacy mode))

; CONFIG5L
  CONFIG  CP0 = OFF             ; Code Protection Block 0 (Block 0 (000800-003FFFh) not code-protected)
  CONFIG  CP1 = OFF             ; Code Protection Block 1 (Block 1 (004000-007FFFh) not code-protected)
  CONFIG  CP2 = OFF             ; Code Protection Block 2 (Block 2 (008000-00BFFFh) not code-protected)
  CONFIG  CP3 = OFF             ; Code Protection Block 3 (Block 3 (00C000-00FFFFh) not code-protected)

; CONFIG5H
  CONFIG  CPB = OFF             ; Boot Block Code Protection bit (Boot block (000000-0007FFh) not code-protected)
  CONFIG  CPD = OFF             ; Data EEPROM Code Protection bit (Data EEPROM not code-protected)

; CONFIG6L
  CONFIG  WRT0 = OFF            ; Write Protection Block 0 (Block 0 (000800-003FFFh) not write-protected)
  CONFIG  WRT1 = OFF            ; Write Protection Block 1 (Block 1 (004000-007FFFh) not write-protected)
  CONFIG  WRT2 = OFF            ; Write Protection Block 2 (Block 2 (008000-00BFFFh) not write-protected)
  CONFIG  WRT3 = OFF            ; Write Protection Block 3 (Block 3 (00C000-00FFFFh) not write-protected)

; CONFIG6H
  CONFIG  WRTC = OFF            ; Configuration Register Write Protection bit (Configuration registers (300000-3000FFh) not write-protected)
  CONFIG  WRTB = OFF            ; Boot Block Write Protection bit (Boot Block (000000-0007FFh) not write-protected)
  CONFIG  WRTD = OFF            ; Data EEPROM Write Protection bit (Data EEPROM not write-protected)

; CONFIG7L
  CONFIG  EBTR0 = OFF           ; Table Read Protection Block 0 (Block 0 (000800-003FFFh) not protected from table reads executed in other blocks)
  CONFIG  EBTR1 = OFF           ; Table Read Protection Block 1 (Block 1 (004000-007FFFh) not protected from table reads executed in other blocks)
  CONFIG  EBTR2 = OFF           ; Table Read Protection Block 2 (Block 2 (008000-00BFFFh) not protected from table reads executed in other blocks)
  CONFIG  EBTR3 = OFF           ; Table Read Protection Block 3 (Block 3 (00C000-00FFFFh) not protected from table reads executed in other blocks)

; CONFIG7H
  CONFIG  EBTRB = OFF           ; Boot Block Table Read Protection bit (Boot Block (000000-0007FFh) not protected from table reads executed in other blocks)

;*******************************************************************************

; Variable Definition
	UDATA
VAR0	RES 1
VAR1	RES 1
VAR2	RES 1
VAR0_MAX RES 1
VAR1_MAX RES 1
VAR2_MAX RES 1
 
UNIT_WAIT RES 1
TEMP_COUNT RES 1
;*******************************************************************************
; Reset Vector
;*******************************************************************************

RES_VECT  CODE    0x0000            ; processor reset vector

    GOTO    BEGINNING                   ; go to beginning of program


;*******************************************************************************
; MAIN PROGRAM
;*******************************************************************************

MAIN_PROG CODE                      ; let linker place main program

BEGINNING
    ; 1 tick = 0.25µs
    ; 1 instruction = 4 ticks
    ; so 1 instruction = 1µs
    MOVLW d'38'
    MOVWF UNIT_WAIT
    
    ; 1377 in decimal
    ; 3.4425e-4 seconds
    ; 0,344 ms
    ;MOVLW 0x02
    ;MOVWF VAR0_MAX
    ;MOVLW 0xFF
    ;MOVWF VAR1_MAX

    ; Init ports routine  
    MOVLB 0xF	    ; Selecting memory bank
    CLRF PORTD	    ; Clear PORTD => output and latches to zero
    CLRF LATD	    ; Clear Latch
    CLRF TRISD	    ; Set port D to output
    CLRF ANSELD	    ; Set port D to digital
    
    ; Set the clock
    ; bit 6-4: clock set to 4Mhz
    ; bit 1-0: clock set to internal oscillator
    MOVLW b'01010010'
    MOVWF OSCCON
    
INITSCREEN
    
; First waiting step (screen powering on)
FIRSTSTEP
    ; We need 400 UNIT_WAIT (> 15ms)
    MOVLW 0x01
    MOVWF VAR1
FIRSTSTEP0
    MOVLW 0x90
    MOVWF VAR0
FIRSTSTEP1
    CALL UNIT_TEMPO
    DECFSZ VAR1
    BRA FIRSTSTEP1
    DECFSZ VAR0
    BRA FIRSTSTEP0
    
; Second step:
; Sending command 000011
; Waiting > 4.1ms (110 UNIT_WAIT)
SECONDSTEP
    MOVLW b'01000011'
    MOVWF LATD
    CALL UNIT_TEMPO
    MOVLW b'00000011'
    MOVWF LATD
    MOVLW d'110'
    MOVWF VAR0
SECONDSTEP0
    CALL UNIT_TEMPO
    DECFSZ VAR0
    BRA SECONDSTEP0

; Third step:
; Sending command "000011"
; Waiting > 100µs (3 UNIT_WAIT)
THIRDSTEP
    MOVLW b'01000011'
    MOVWF LATD
    CALL UNIT_TEMPO
    MOVLW b'00000011'
    MOVWF LATD
    MOVLW d'110'
    MOVWF VAR0
THIRDSTEP0
    CALL UNIT_TEMPO
    DECFSZ VAR0
    BRA THIRDSTEP0
    
; Fourth step:
; Sending command "000011"
; Sending command "000010" (set to 4 bits)
; Sending command "000010" (set to 4 bits) (again ?)
; Sending command "001100" (2 lines, 11 points font)
FOURTHSTEP
    MOVLW b'01000011'
    MOVWF LATD
    CALL UNIT_TEMPO
    MOVLW b'00000011'
    MOVWF LATD
    
    MOVLW b'01000010'
    MOVWF LATD
    CALL UNIT_TEMPO
    MOVLW b'00000010'
    MOVWF LATD
    
    MOVLW b'01000010'
    MOVWF LATD
    CALL UNIT_TEMPO
    MOVLW b'00000010'
    MOVWF LATD
    
    MOVLW b'01001100'
    MOVWF LATD
    CALL UNIT_TEMPO
    MOVLW b'00001100'
    MOVWF LATD
    
    GOTO $                          ; loop forever
    

;------------------------------------------------
; Wait for 38µs
;------------------------------------------------
    
UNIT_TEMPO
    ; Load UNIT_WAIT into a temp variable
    MOVF UNIT_WAIT
    MOVWF TEMP_COUNT
    
LOOPTEMP
    DECFSZ TEMP_COUNT
    BRA LOOPTEMP
    RETURN

    END