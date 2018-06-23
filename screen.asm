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
  CONFIG  PBADEN = OFF           ; PORTB A/D Enable bit (PORTB<5:0> pins are configured as analog input channels on Reset)
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
INT_VAR        UDATA_ACS
UNIT_WAIT RES 1

; Compteurs pour temporisation soft
VAR0	RES 1
VAR1	RES 1

DATA_INS    RES 1
TEMP_COUNT RES 1
NUMBER	RES 1
CHAR	RES 1

; Variables pour l'acquisition
RESULTHI  RES 1
RESULTLO  RES 1
  
  
DIGIT_HI    RES 1
DIGIT_LO    RES 1
REST_HI    RES 1
REST_LO    RES 1
UNITS	    RES 1
TENS    RES 1
HUNDREDTHS   RES 1
THOUSANDTHS    RES 1
BOOL	    RES 1
	   
DEAD_WEIGHT RES 1
 
QUOTIENTHI  RES 1
QUOTIENTLO  RES 1 


;*******************************************************************************
; Reset Vector
;*******************************************************************************

RES_VECT  CODE    0x0000            ; processor reset vector

    GOTO    BEGINNING                   ; go to beginning of program


; INTERRUPT ROUTINE
ISRHV     CODE    0x0008
    GOTO    HIGH_ISR

ISRH      CODE
HIGH_ISR
    ; Pour l'instant ne fait qu'afficher 7
    BCF INTCON, 1
    CALL TARE
    RETFIE  FAST

;*******************************************************************************
; MAIN PROGRAM
;*******************************************************************************
    
TARE
    CALL ACQUISITION
    MOVF RESULTLO, 0
    MOVWF DEAD_WEIGHT
    RETURN

;------------------------------------------------
; PIC configuration routine
;------------------------------------------------    
INIT
    ; 1 tick = 0.25µs
    ; 1 instruction = 4 ticks
    ; so 1 instruction = 1µs
    MOVLW d'11'
    MOVWF UNIT_WAIT
    
    
    ; Init DEAD_WEIGHT value at 0    
    MOVLW 0x00
    MOVWF DEAD_WEIGHT

    ; Init ports routine  
    ;MOVLB 0xF	    ; Selecting memory bank
    CLRF PORTD	    ; Clear PORTD => output and latches to zero
    CLRF LATD	    ; Clear Latch
    CLRF TRISD	    ; Set port D to output
    CLRF ANSELD	    ; Set port D to digital
    
    CLRF PORTB
    CLRF LATB
    SETF TRISB
    CLRF ANSELB
    
    ; Set the clock
    ; bit 6-4: clock set to 4Mhz
    ; bit 1-0: clock set to internal oscillator
    MOVLW b'01010010'
    MOVWF OSCCON
    
    ; Réglage de INTCON
    ; Bit 7: Activer les interruptions globales
    ; Bit 6: Activer les interruptions des périphériques
    ; Bit 4: Activer l'interruption sur INT0
    BSF INTCON, 4
    BSF INTCON, 7
    BSF INTCON, 6
    ; Réglage de INTCON2
    ; Bit 6 = 0: Interruption sur front descendant
    BSF INTCON2, 7
    BCF INTCON2, 6
    BCF INTCON, 2
    RETURN

;------------------------------------------------
; Calibration routine for LCD screen
;------------------------------------------------    
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
    DECFSZ VAR0
    BRA FIRSTSTEP1
    DECFSZ VAR1
    BRA FIRSTSTEP0

    
; Second step:
; Sending command 000011
; Waiting > 4.1ms (110 UNIT_WAIT)
SECONDSTEP
    MOVLW b'01000011'
    CALL VALIDATECMD
    
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
    CALL VALIDATECMD
    
    MOVLW d'10'
    MOVWF VAR0
THIRDSTEP0
    CALL UNIT_TEMPO
    DECFSZ VAR0
    BRA THIRDSTEP0
    
; Fourth step:
; Sending command "000011"
;
; Sending command "000010" (set to 4 bits)
; Sending command "000010" (set to 4 bits) (again ?)
; Sending command "001100" (2 lines, 8 points font, comme M.Lambert)
; Sending command "000000" (display on)
; Sending command "001110" (cursor appears)
FOURTHSTEP
    ; 4 bits instruction for 4 bit mode
    MOVLW b'01000011'
    CALL VALIDATECMD 
    
    MOVLW b'01000010'
    CALL VALIDATECMD 
    
    MOVLW b'01000010'
    CALL VALIDATECMD 
    
    MOVLW b'01001000'
    CALL VALIDATECMD 
    
    ; 8 BITS COMMANDS FOLLOWING
    
    ;DISPLAY OFF
    MOVLW b'01000000'
    CALL VALIDATECMD 
    
    MOVLW b'01001000'
    CALL VALIDATECMD 
    
    ; DISPLAY/CURSOR/BLINKING ON/OFF
    MOVLW b'01000000'
    CALL VALIDATECMD 
    
    ; 0 -> Blinking
    ; 1 -> Cursor
    ; 2 -> Display
    MOVLW b'01001111'
    CALL VALIDATECMD 
    
    CALL CLEARDISPLAY
    
    ; ENTRY SET MODE
    ; I/D = 1 => Increment
    ; S => The display does not shift
    MOVLW b'01000000'
    CALL VALIDATECMD
    
    MOVLW b'01000110'
    CALL VALIDATECMD
    
    RETURN

;------------------------------------------------
; Polls the CAN for a new value
;------------------------------------------------
ACQUISITION
    CLRF PORTA
    
    MOVLW b'10101110'
    MOVWF ADCON2
    MOVLW b'00000000'
    MOVWF ADCON1
    
    BSF TRISA, 0      ; Set RA to input
    BSF ANSELA, 0     ; Set RA to analog
    
    MOVLW b'00000001' ; Only RA0 will be activated
    MOVWF ADCON0
    
LAUNCH
    BSF ADCON0,GO
    
POLL
    BTFSC ADCON0,GO
    BRA POLL
    
    ; If a new value has been captured, move the result in global vars
    MOVFF ADRESH, RESULTHI
    MOVFF ADRESL, RESULTLO
    
    RETURN

;------------------------------------------------
; Helper routine to bubble carry from units to tens
;------------------------------------------------
UNITS_CARRY
    INCF TENS
    MOVLW d'10'
    CPFSLT TENS
    CALL TENS_CARRY
    
    SUBWF UNITS
    RETURN

;------------------------------------------------
; Helper routine to bubble carry from tens to hundredths
;------------------------------------------------    
TENS_CARRY
    INCF HUNDREDTHS
    MOVLW d'10'
    CPFSLT HUNDREDTHS
    CALL HUNDREDTHS_CARRY
    
    SUBWF TENS
    RETURN

;------------------------------------------------
; Helper routine to bubble carry from hundredths to thousandths
;------------------------------------------------    
HUNDREDTHS_CARRY
    INCF THOUSANDTHS
    MOVLW d'10'
    SUBWF HUNDREDTHS
    RETURN

;------------------------------------------------
; Helper routine to add packets of 256 to the counters
;------------------------------------------------    
ADD_256
    MOVLW d'2'
    ADDWF HUNDREDTHS
    MOVLW d'9'
    CPFSLT HUNDREDTHS
    CALL HUNDREDTHS_CARRY
    
    MOVLW d'5'
    ADDWF TENS
    MOVLW d'9'
    CPFSLT TENS
    CALL TENS_CARRY
    
    MOVLW d'6'
    ADDWF UNITS
    MOVLW d'9'
    CPFSLT UNITS
    CALL UNITS_CARRY
    RETURN
    
;------------------------------------------------
; Helper routine to add packets of 512 to the counters
;------------------------------------------------    
ADD_512
    CALL ADD_256
    CALL ADD_256
    RETURN

;------------------------------------------------
; Divide a number to display in radix 10
; The results are stored in global variables :
;   UNITS, TENS, HUNDREDTHS and THOUSANDTHS
;------------------------------------------------    
RADIX_10
    ; Memory clear
    MOVLW 0x00
    MOVWF UNITS
    MOVWF TENS
    MOVWF HUNDREDTHS
    MOVWF THOUSANDTHS
    MOVWF REST_HI
    MOVWF REST_LO
    
    ; Storing numbers to divide
    MOVF RESULTHI, 0
    MOVWF REST_HI          
    
    MOVF RESULTLO, 0
    MOVWF REST_LO
    

    ; Inspecting if Most Significant Byte is zero
    ; If MSByte are != 0 => divide by 256 packets

    MOVF REST_HI, 0
    ANDLW b'00000010'		; 2^9 bit is inspected
    MOVWF BOOL
    TSTFSZ BOOL
    CALL ADD_512
    
    MOVF REST_HI, 0
    ANDLW b'00000001'		; 2^8 bit is inspected
    MOVWF BOOL
    TSTFSZ BOOL
    CALL ADD_256	    
    
; Dividing by 100 packets
DIV_HUNDREDTHS
    MOVLW d'10'
    CPFSLT HUNDREDTHS		; Checking if HUNDREDTHS >= 10
    CALL HUNDREDTHS_CARRY	; If it is, bubble the carry to thousandths
    INCF HUNDREDTHS
    
    MOVLW d'100'
    SUBWF REST_LO, 1		; Sub 100 to the rest
    BN NEXTSTEP_HUNDREDTHS	; If we subbed to much, skip
    BRA DIV_HUNDREDTHS
    

NEXTSTEP_HUNDREDTHS
    BC DIV_HUNDREDTHS
    DECF HUNDREDTHS
    ADDWF REST_LO, 1		; Putting back the missing 100
    
; Dividing by tens packets    
DIV_TENS
    MOVLW d'10'
    CPFSLT TENS			; Checking if TENS >= 10
    CALL TENS_CARRY		; If it is, bubble the carry to hundredths
    INCF TENS

    
    SUBWF REST_LO, 1		; Sub 100 to the rest
    BN NEXTSTEP_TENS		; If we subbed to much, skip
    BRA DIV_TENS
    
NEXTSTEP_TENS
    DECF TENS
    ADDWF REST_LO, 1		; Putting back the missing 100
    
; Putting the rest as unit
    MOVF REST_LO, 0
    ADDWF UNITS
    MOVLW d'10'
    CPFSLT UNITS
    CALL UNITS_CARRY		; If UNITS >= 10, bubble the carry to tens
    
    RETURN

; If QUOTIENTLO is about to overflow, increment QUOTIENTHI and clear QUOTIENTLO
OVERFLOW_25
    INCF QUOTIENTHI
    MOVLW 0x00
    MOVWF QUOTIENTLO
    GOTO DIV_25_CONTINUE

;------------------------------------------------
; Divide by 25 on two bytes to two bytes
;   Values have been tested until a max of 27,648
;    = 1024 * 27, the max value that will be passed to this
;    routine in this program
;   
;   This routine divides the two bytes stored in RESULTHI and RESULTLO
;   and stores the results in QUOTIENTHI:QUOTIENTLO
;------------------------------------------------    
DIVIDE_25
    ; Clearing working registers
    MOVLW 0x00
    MOVWF QUOTIENTHI
    MOVWF QUOTIENTLO
    
    ; Copying values in working variables
    MOVF RESULTHI, 0
    MOVWF REST_HI
    MOVF RESULTLO, 0
    MOVWF REST_LO
    
DIV_25
    ; Checking for overflow of QUOTIENTLO
    MOVLW 0xFF
    CPFSLT QUOTIENTLO
    GOTO OVERFLOW_25
    INCF  QUOTIENTLO		; If no overflow, increment QUOTIENTLO
    
DIV_25_CONTINUE
    
    MOVLW d'25'
    SUBWF REST_LO, 1		; Sub 25 to the rest
    BN NEXTSTEP_DIV_25		; If we subbed to much, skip
    BRA DIV_25

NEXTSTEP_DIV_25
; The program is here because negative flag was raised
; To test if it is a true negative, and not a 2's complement negative,
;   the carry flag is also tested
    BNC NEXTNEXTSTEP_DIV_25	
    BRA DIV_25
NEXTNEXTSTEP_DIV_25
; The negative AND carry flags are raised, so it is a true negative
    MOVLW 0x00
    CPFSGT REST_HI		; Checking if the high rest is empty
    BRA NEXTNEXTNEXTSTEP_DIV_25
    DECF REST_HI		; If it is not, decrement high rest
    BRA DIV_25			; Continue with substractions
NEXTNEXTNEXTSTEP_DIV_25
; Program is here when negative AND carry flags are raised AND
;   the high rest is empty : it is no longer possible to sub 25    
    MOVLW d'25'
    ADDWF REST_LO		; adding the missing 25
    DECF QUOTIENTLO		; decrementing the quotient of the excedent 25
    MOVLW d'13'
    CPFSLT REST_LO		; Rounding up if rest >= 13
    BRA ROUND_25
    
    BRA END_25
    
ROUND_25
    INCF QUOTIENTLO

END_25
    RETURN

;------------------------------------------------
; Uses the MULWF to multiply RESULTHI:RESULTLO by 27
;    Stores the results in RESULTHI:RESULTO up to 1024*27 = 27648    
;------------------------------------------------    
MUL_27
    MOVLW d'27'
    MULWF RESULTHI
    MOVF PRODL, 0
    MOVWF RESULTHI
    
    MOVLW d'27'
    MULWF RESULTLO
    MOVF PRODH, 0
    ADDWF RESULTHI, 1
    MOVF PRODL, 0
    MOVWF RESULTLO
    
    RETURN

;------------------------------------------------
; Routine to convert 5/1024 Volts in grams with a 27/25 = 1.08 ratio
;    Uses the MUL_27 and DIVIDE_25 routines
;   The global variables used to store values are RESULTHI:RESULTLO
;------------------------------------------------    
APPLY_RATIO
    CALL MUL_27
    CALL DIVIDE_25
    MOVF QUOTIENTLO, 0
    MOVWF RESULTLO
    MOVF QUOTIENTHI, 0
    MOVWF RESULTHI
    RETURN
    
;------------------------------------------------
; Wait for 38µs
;------------------------------------------------
    
UNIT_TEMPO
    ; Load UNIT_WAIT into a temp variable
    MOVF UNIT_WAIT, 0
    MOVWF TEMP_COUNT
    
LOOPTEMP
    DECFSZ TEMP_COUNT
    BRA LOOPTEMP
    RETURN

; Dead code ?   
;SEND_INSTRUCTION
;    MOVWF DATA_INS
    

;------------------------------------------------
; Validate a command to the LCD
; Set the desired command in W before calling
;------------------------------------------------
    
VALIDATECMD
    MOVWF LATD
    CALL UNIT_TEMPO
    BCF LATD, 6
    CALL UNIT_TEMPO
    RETURN
    
;------------------------------------------------
; Clear the display
;------------------------------------------------
    
CLEARDISPLAY
    ; DISPLAY CLEAR
    MOVLW b'01000000'
    CALL VALIDATECMD
    
    MOVLW b'01000001'
    CALL VALIDATECMD
    
    ; WAIT > 1.52 ms
    MOVLW d'250'
    MOVWF VAR0
CLEARDISPLAYWAIT
    CALL UNIT_TEMPO
    DECFSZ VAR0
    BRA CLEARDISPLAYWAIT
    
    RETURN
    
;------------------------------------------------
; Write CRLF
;------------------------------------------------
    
WRITECRLF
    ;CRLF
    ;First line : "10000000"
    ;Second line: "11000000"
    ;Third line:  "10010100"
    MOVLW b'01001100'
    CALL VALIDATECMD
    
    MOVLW b'01000000'
    CALL VALIDATECMD
    
    RETURN
    
;------------------------------------------------
; Move cursor to line 1 (Line 0 is the first row)
;------------------------------------------------    
TOLINE1
    MOVLW b'01001100'
    CALL VALIDATECMD
    
    MOVLW b'01000000'
    CALL VALIDATECMD
    
    RETURN
    
;------------------------------------------------
; Move cursor to line 2 (Line 0 is the first row)
;------------------------------------------------  
TOLINE2
    MOVLW b'01001100'
    CALL VALIDATECMD
    
    MOVLW b'01000000'
    CALL VALIDATECMD
    
    RETURN
    
;------------------------------------------------
; Move cursor to line 3 (Line 0 is the first row)
;------------------------------------------------ 
TOLINE3
    MOVLW b'01001001'
    CALL VALIDATECMD
    
    MOVLW b'01000100'
    CALL VALIDATECMD
    
    RETURN

;------------------------------------------------
; Write W
;------------------------------------------------

WRITEW
    MOVLW b'01010101'
    CALL VALIDATECMD
    
    MOVLW b'01010111'	
    CALL VALIDATECMD
    
    RETURN
    
;------------------------------------------------
; Write E
;------------------------------------------------

WRITEE
    MOVLW b'01010100'
    CALL VALIDATECMD
    
    MOVLW b'01010101'	
    CALL VALIDATECMD
    
    RETURN
    
;------------------------------------------------
; Write L
;------------------------------------------------

WRITEL
    MOVLW b'01010100'
    CALL VALIDATECMD
    
    MOVLW b'01011100'	
    CALL VALIDATECMD
    
    RETURN
    
;------------------------------------------------
; Write C
;------------------------------------------------

WRITEC
    MOVLW b'01010100'
    CALL VALIDATECMD
    
    MOVLW b'01010011'	
    CALL VALIDATECMD
    
    RETURN
    
;------------------------------------------------
; Write O
;------------------------------------------------

WRITEO
    MOVLW b'01010100'
    CALL VALIDATECMD
    
    MOVLW b'01011111'	
    CALL VALIDATECMD
    
    RETURN
    
;------------------------------------------------
; Write M
;------------------------------------------------

WRITEM 
    MOVLW b'01010100'
    CALL VALIDATECMD
    
    MOVLW b'01011101'	
    CALL VALIDATECMD
    
    RETURN
    
;------------------------------------------------
; Write G
;------------------------------------------------

WRITEG 
    MOVLW b'01010110'
    CALL VALIDATECMD
    
    MOVLW b'01010111'	
    CALL VALIDATECMD
    
    RETURN
    
;------------------------------------------------
; Write 0
;------------------------------------------------

WRITE0
    MOVLW b'01010011'
    CALL VALIDATECMD
    
    MOVLW b'01010000'	
    CALL VALIDATECMD
    
    RETURN
    
;------------------------------------------------
; Write 1
;------------------------------------------------

WRITE1
    MOVLW b'01010011'
    CALL VALIDATECMD
    
    MOVLW b'01010001'	
    CALL VALIDATECMD
    
    RETURN
    
;------------------------------------------------
; Write 2
;------------------------------------------------

WRITE2
    MOVLW b'01010011'
    CALL VALIDATECMD
    
    MOVLW b'01010010'	
    CALL VALIDATECMD
    
    RETURN
    
;------------------------------------------------
; Write 3
;------------------------------------------------

WRITE3
    MOVLW b'01010011'
    CALL VALIDATECMD
    
    MOVLW b'01010011'	
    CALL VALIDATECMD
    
    RETURN
    
;------------------------------------------------
; Write 4
;------------------------------------------------

WRITE4
    MOVLW b'01010011'
    CALL VALIDATECMD
    
    MOVLW b'01010100'	
    CALL VALIDATECMD
    
    RETURN
    
;------------------------------------------------
; Write 5
;------------------------------------------------

WRITE5
    MOVLW b'01010011'
    CALL VALIDATECMD
    
    MOVLW b'01010101'	
    CALL VALIDATECMD
    
    RETURN
    
;------------------------------------------------
; Write 6
;------------------------------------------------

WRITE6
    MOVLW b'01010011'
    CALL VALIDATECMD
    
    MOVLW b'01010110'	
    CALL VALIDATECMD
    
    RETURN
    
;------------------------------------------------
; Write 7
;------------------------------------------------

WRITE7
    MOVLW b'01010011'
    CALL VALIDATECMD
    
    MOVLW b'01010111'	
    CALL VALIDATECMD
    RETURN
    
;------------------------------------------------
; Write 8
;------------------------------------------------

WRITE8
    MOVLW b'01010011'
    CALL VALIDATECMD
    
    MOVLW b'01011000'	
    CALL VALIDATECMD
    
    RETURN
    
;------------------------------------------------
; Write 9
;------------------------------------------------

WRITE9
    MOVLW b'01010011'
    CALL VALIDATECMD
    
    MOVLW b'01011001'	
    CALL VALIDATECMD
    
    RETURN
    
    
;------------------------------------------------
; Write number
; Set W to desired number before calling this function
; This is going to change the content in W
;------------------------------------------------

WRITENUMBER
    MOVWF NUMBER
    MOVLW b'01010011'
    CALL VALIDATECMD
    
    MOVLW b'01010000'
    IORWF NUMBER, 0
    CALL VALIDATECMD
    
    RETURN
    
WRITECHAR
    MOVWF NUMBER
    MOVWF CHAR
    RRCF CHAR, 1
    RRCF CHAR, 1
    RRCF CHAR, 1
    RRCF CHAR, 1
    MOVLW b'00001111'
    ANDWF CHAR, 1
    MOVLW b'01010000'
    IORWF CHAR, 0
    ;MOVLW b'01010011'
    CALL VALIDATECMD
    
    MOVLW b'00001111'
    ANDWF NUMBER, 1
    MOVLW b'01010000'
    IORWF NUMBER, 0
    CALL VALIDATECMD
    
    RETURN

    
MAIN_PROG CODE                      ; let linker place main program

BEGINNING
    CALL INIT
    CALL INITSCREEN
    ; Tare the dead weight
    CALL TARE
   
SHOWACQ
    CALL CLEARDISPLAY
    CALL ACQUISITION		    ; Retrieving CAN values
    MOVF DEAD_WEIGHT, 0		    ; Applying tare
    SUBWF RESULTLO, 1
    BN NEG_VALUE		    ; If value is just below 0, display 0g
    
    
    CALL APPLY_RATIO		    ; Grams conversion
    CALL RADIX_10		    ; Division in Units, tens, etc.
    
    ; Display the thousandths digit
    MOVF THOUSANDTHS, 0
    CALL WRITENUMBER
    
    ; Display the hundredths digit
    MOVF HUNDREDTHS, 0
    CALL WRITENUMBER
    
    ; Display the tens digit
    MOVF TENS, 0
    CALL WRITENUMBER
    
    ; Display the units digits
    MOVF UNITS, 0
    CALL WRITENUMBER
    
    ; Write the g of grams
    CALL WRITEG

; Other temporisation routine to avoid char flicker
LONG_WAITING
    MOVLW 0x20
    MOVWF VAR1
SHOWACQWAIT0
    MOVLW 0xFF
    MOVWF VAR0
SHOWACQWAIT1
    CALL UNIT_TEMPO
    DECFSZ VAR0
    BRA SHOWACQWAIT1
    DECFSZ VAR1
    BRA SHOWACQWAIT0
    
        
    BRA SHOWACQ
               
    GOTO $                          ; loop forever
    
NEG_VALUE
    CALL WRITE0
    CALL WRITE0
    CALL WRITE0
    CALL WRITE0
   
    CALL WRITEG
   
    BRA LONG_WAITING
    
    END