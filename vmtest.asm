init
(ldi r0 0)
(ldi r1 1)
(ldi r2 2)
(ldi r3 3)
(ldi r4 4)
(ldi r5 5)
(ldi r6 6)
(ldi r7 7)
(ldi r8 8)
(ldi r9 9)
(ldi r10 10)
(ldi r11 11)
(ldi r12 12)
(ldi r13 13)
(ldi r14 14)
(ldi r15 15)
(ldi r16 16)
(ldi r17 17)
(ldi r18 18)
(ldi r19 19)
(ldi r20 20)
(ldi r21 21)
(ldi r22 22)
(ldi r23 23)
(ldi r24 24)
(ldi r25 25)
(ldi r26 26)
(ldi r27 27)
(ldi r28 28)
(ldi r29 29)
(ldi r30 30)
(sys r20 3)
not
(not r30 r30)
(sys r30 3)
sdiv
(sdiv r30 r30 r7)
(sys r30 3)
fdiv-itf-fti
(itf r2 r2)
(fti r2 r2)
(itf r2 r2)
(itf r3 r3)
(fdiv r3 r3 r2)
(sys r3 3)
mov
(mov r4 r8)
(sys r4 3)
(sys r8 3)
st-ld
(ldi r0 100)
(st r0 r1)
(ld r2 r0)
(sys r2 3)
conditional-1
(ldi r0 0)
(ldi r1 cond-cond)
(jnz r1 r0)
cond-cont
(ldi r3 3)
(ldi r31 cond-final)
cond-cond
(ldi r3 4)
cond-final
(sys r3 3)
conditional-2
(ldi r0 1)
(ldi r1 cond-cond2)
(jnz r1 r0)
cond-cont2
(ldi r3 3)
(ldi r31 cond-final2)
cond-cond2
(ldi r3 4)
cond-final2
(sys r3 3)
(sys r0 1)
