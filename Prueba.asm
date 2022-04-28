.data
numero: .asciiz "Ingrese el numero a multiplicar por 10: "
number: .word 0
result: .word 0
.text
    main:
        # Se le solicita al usuario el valor
        li $v0, 4
        la $a0, numero
        syscall
        # Se guarda el valor
        li $v0, 5
        syscall
        # Se guarda en memoria
        sw $v0, number
        # Se llama la función de multiplicacion
        addi $s2, $s2, 0
        li $a0, 0
        lw $s3, number
        jal multiplicacion10
        sw $a0, result
        # Se imprime el resultado
        li $v0, 1
        lw $a0, result
        syscall
        # Se termina el programa
        li $v0, 10
        syscall
 #--------------------- Funciones ---------------------# 
multiplicacion10:
              # Se reserva en memoria 2 palabras
              subu $sp, $sp, 8
              sw $ra, ($sp)
              sw $s1, 4($sp)
              # Caso Base
              beq $a0, 10, multiplicacion10Fin
              # Caso recursivo
              add $s0, $s0, $s3
              addi $a0, $a0, 1
              jal multiplicacion10
              move $a0, $s0
              multiplicacion10Fin:
                       lw $ra, ($sp)
                       lw $s1, 4($sp)
                       addu $sp, $sp, 8
                       jr $ra