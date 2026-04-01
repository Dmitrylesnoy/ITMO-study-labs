    .data

input_addr:      .word  0x80
output_addr:     .word  0x84

    .text

    \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

_start:
    @p input_addr a! @       \ a:[]
    @p input_addr a! @       \ b:[]

    gcd_while

    @p output_addr a! !
    halt

    \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

gcd_while:                   \ b:a:[]   
    dup if gcd_finish        \ if b==0 : finish

    dup a! over              \ A=b ; a:b:[]
    modulo                   \ a:b:[]

    gcd_while ;

sub_A:
    a inv lit 1 + +          \ T-a:[]
    ;

modulo:
    sub_A                    \ a-b:b:[]
    dup -if modulo           \ if (a-b)>0 : new iter
    a +
    ;

gcd_finish:
    drop                     \ n:acc:[]
    ;

    
